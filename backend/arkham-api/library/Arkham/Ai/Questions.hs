{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : Arkham.Ai.Questions
Description : Non-blocking questions an AI seat poses to its teammates.

The AI seats already auto-drive their own parked questions (see
'Arkham.Ai.Decision'). This module is the /other/ direction: a read-only
assessment that lets an AI seat surface advisory, NON-BLOCKING questions for the
table. Two kinds:

  * @offerHelp@ — an AI investigator who is combat-capable and can reach a
    teammate's tough engaged enemy offers to move in and fight it.
  * @offerCommit@ — while a skill test is open, an AI seat that is not the
    performer and holds a legally committable card offers to commit its single
    best (most-matching-icons) card to boost the performer's test.

Nothing here mutates game state. 'assessAiQuestions' is evaluated against a pure
@ReaderT Game Identity@ snapshot from a read-only GET handler, exactly like
'Arkham.Ai.Decision.decideAi'. Each 'AiQuestion' carries the real
'Arkham.Message.Message' values its options would enqueue; the frontend forwards
the chosen option's messages verbatim to the raw channel (they round-trip
through 'Message'\'s existing @{tag, contents}@ JSON), so accepting an offer
simply applies 'AddAiPriority' + 'SetAiFocusOverride' to the /asker/'s seat —
reusing the priority\/movement routing that already steers a seat toward a
priority enemy.
-}
module Arkham.Ai.Questions (
  AiQuestion (..),
  AiQuestionOption (..),
  assessAiQuestions,
) where

import Arkham.Prelude

import Arkham.Ai.Focus (Focus (CombatFocus))
import Arkham.Ai.Helpers (getAiPlayerState)
import Arkham.Ai.State (AiPlayerState (..))
import Arkham.Ai.Tags (abDamage, ctAbilities, lookupCardTag)
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Card (Card, toCardId)
import Arkham.Card.CardCode (toCardCode)
import Arkham.Card.CardDef (cdSkills, toCardDef)
import Arkham.Classes.HasGame (HasGame, getGame)
import Arkham.Classes.Query (select, selectOne)
import Arkham.Distance (unDistance)
import Arkham.Enemy.Types (Field (EnemyCard, EnemyFight, EnemyHealth, EnemyHealthDamage, EnemyLocation))
import Arkham.Game.Base (Game (gameSettings))
import Arkham.Game.Settings (Settings (settingsAiPlayers))
import Arkham.GameEnv (getDistance, getSkillTest)
import Arkham.Helpers.Investigator (getMaybeLocation, modifiedStatsOf)
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Helpers.SkillTest (
  getCommittableCards,
  getSkillTestDifficulty,
  getSkillTestInvestigator,
  getSkillTestMatchingSkillIcons,
  getSkillTestModifiedSkillValue,
 )
import Arkham.Id (EnemyId, InvestigatorId, LocationId, PlayerId)
import Arkham.Investigator.Types (Field (InvestigatorName, InvestigatorPlayerId))
import Arkham.Matcher (assetControlledBy, enemyEngagedWith, notInvestigator, pattern InvestigatorIsPlayer)
import Arkham.Matcher.Location (LocationMatcher (LocationWithId))
import Arkham.Message (Message (AddAiPriority, SetAiFocusOverride), pattern SkillTestCommitCard)
import Arkham.Name (toName, toTitle)
import Arkham.Projection (field)
import Arkham.SkillType (SkillIcon, SkillType (SkillCombat))
import Arkham.Source (Source (GameSource))
import Arkham.Stats (statsSkillValue)
import Arkham.Target (Target (EnemyTarget))
import Arkham.Tracing (Tracing)
import Data.Map.Strict qualified as Map

-- | One selectable answer for an 'AiQuestion'. Picking it forwards 'messages'
-- (real 'Message' values) verbatim to the raw channel.
data AiQuestionOption = AiQuestionOption
  { label :: Text
  , messages :: [Message]
  }
  deriving stock Show

instance ToJSON AiQuestionOption where
  toJSON o =
    object
      [ "label" .= o.label
      , "messages" .= o.messages
      ]

-- | A non-blocking question an AI seat poses to (or auto-answers for) a
-- teammate. The frontend fetches these via a read-only GET and surfaces them
-- without ever blocking the game queue.
data AiQuestion = AiQuestion
  { id :: Text
  -- ^ stable dedup key, e.g. @offerHelp:<fromPid>:<toPid>:<enemyId>@.
  , kind :: Text
  , fromPlayer :: PlayerId
  , fromInvestigator :: InvestigatorId
  , fromName :: Text
  , toPlayer :: PlayerId
  , toInvestigator :: InvestigatorId
  , toName :: Text
  , toIsAi :: Bool
  , prompt :: Text
  , options :: [AiQuestionOption]
  , aiAnswer :: Maybe Int
  -- ^ when 'toIsAi', the option index the AI target auto-picks.
  }
  deriving stock Show

instance ToJSON AiQuestion where
  toJSON q =
    object
      [ "id" .= q.id
      , "kind" .= q.kind
      , "fromPlayer" .= q.fromPlayer
      , "fromInvestigator" .= q.fromInvestigator
      , "fromName" .= q.fromName
      , "toPlayer" .= q.toPlayer
      , "toInvestigator" .= q.toInvestigator
      , "toName" .= q.toName
      , "toIsAi" .= q.toIsAi
      , "prompt" .= q.prompt
      , "options" .= q.options
      , "aiAnswer" .= q.aiAnswer
      ]

{- | Assess every ENABLED AI seat and produce its advisory questions. Read-only:
intended to be run against a pure @ReaderT Game Identity@ snapshot (which is
both 'HasGame' and 'Tracing'), exactly like 'Arkham.Ai.Decision.decideAi'.
Questions are deduplicated by their stable 'id'.
-}
assessAiQuestions :: (HasGame m, Tracing m) => m [AiQuestion]
assessAiQuestions = do
  g <- getGame
  let enabledAiSeats =
        [pid | (pid, st) <- Map.toList (settingsAiPlayers (gameSettings g)), aiEnabled st]
  offerHelps <- concat <$> traverse offerHelpForSeat enabledAiSeats
  offerCommits <- offerCommitQuestions enabledAiSeats
  pure (dedupById (offerHelps <> offerCommits))

-- | Keep the first question seen per 'id'.
dedupById :: [AiQuestion] -> [AiQuestion]
dedupById = go mempty
 where
  go _ [] = []
  go seen (q : qs)
    | q.id `elem` seen = go seen qs
    | otherwise = q : go (q.id : seen) qs

-- * Offer help

{- | The offer-help questions a single AI seat (the asker) raises. The asker
must control an investigator and be combat-capable; otherwise it has no help to
offer and we short-circuit to no questions.
-}
offerHelpForSeat :: (HasGame m, Tracing m) => PlayerId -> m [AiQuestion]
offerHelpForSeat aiPid =
  selectOne (InvestigatorIsPlayer aiPid) >>= \case
    Nothing -> pure []
    Just aiIid -> do
      aiHere <- getMaybeLocation aiIid
      aiStats <- modifiedStatsOf Nothing aiIid
      let combat = statsSkillValue aiStats SkillCombat
      aiWeapon <- bestWeaponDamage aiIid
      let combatCapable = aiWeapon >= 2 || combat >= 3
      if not combatCapable
        then pure []
        else do
          fromName <- toTitle <$> field InvestigatorName aiIid
          teammates <- select (notInvestigator aiIid)
          concat <$> traverse (offersForTeammate aiPid aiIid aiHere fromName) teammates

{- | The offer-help questions the asker raises for one teammate: one per tough
engaged enemy the asker can reach.
-}
offersForTeammate
  :: (HasGame m, Tracing m)
  => PlayerId
  -- ^ asker seat
  -> InvestigatorId
  -- ^ asker investigator
  -> Maybe LocationId
  -- ^ asker location
  -> Text
  -- ^ asker name
  -> InvestigatorId
  -- ^ teammate
  -> m [AiQuestion]
offersForTeammate aiPid aiIid aiHere fromName tmIid = do
  tmPid <- field InvestigatorPlayerId tmIid
  tmStats <- modifiedStatsOf Nothing tmIid
  let tmCombat = statsSkillValue tmStats SkillCombat
  tmWeapon <- bestWeaponDamage tmIid
  tmName <- toTitle <$> field InvestigatorName tmIid
  toIsAi <- maybe False aiEnabled <$> getAiPlayerState tmPid
  enemies <- select (enemyEngagedWith tmIid)
  fmap catMaybes $ for enemies $ \eid -> do
    mFight <- field EnemyFight eid
    mHealth <- field EnemyHealth eid
    dmg <- field EnemyHealthDamage eid
    eLoc <- field EnemyLocation eid
    let
      -- Same `EnemyHealth - EnemyHealthDamage` remaining as Decision.hs.
      remaining = fmap (subtract dmg) mHealth
      toughForTm = case remaining of
        Just rh -> rh > tmWeapon && (maybe True (tmCombat <) mFight || rh > tmWeapon * 2)
        Nothing -> False
    canHelp <- if toughForTm then askerCanReach aiIid aiHere eLoc else pure False
    if toughForTm && canHelp
      then do
        enemyName <- toTitle <$> field EnemyCard eid
        pure . Just
          $ mkOfferHelp aiPid aiIid fromName tmPid tmIid tmName toIsAi toughForTm eid enemyName
      else pure Nothing

{- | Whether the asker can reach the enemy: colocated, within 2 by graph
distance, or one hop away. Any missing location (the enemy is not on the map, no
path is known) is treated as unreachable.
-}
askerCanReach
  :: (HasGame m, Tracing m) => InvestigatorId -> Maybe LocationId -> Maybe LocationId -> m Bool
askerCanReach _ _ Nothing = pure False
askerCanReach aiIid aiHere (Just eLoc)
  | aiHere == Just eLoc = pure True
  | otherwise = do
      withinTwo <- case aiHere of
        Just here -> maybe False ((<= 2) . unDistance) <$> getDistance here eLoc
        Nothing -> pure False
      if withinTwo
        then pure True
        else notNull <$> getCanMoveToMatchingLocations aiIid GameSource (LocationWithId eLoc)

-- | Build the offer-help question. Accepting (option 0) makes the ASKER seat
-- prioritize and focus on fighting the enemy.
mkOfferHelp
  :: PlayerId
  -> InvestigatorId
  -> Text
  -> PlayerId
  -> InvestigatorId
  -> Text
  -> Bool
  -> Bool
  -> EnemyId
  -> Text
  -> AiQuestion
mkOfferHelp aiPid aiIid fromName tmPid tmIid tmName toIsAi toughForTm eid enemyName =
  AiQuestion
    { id = "offerHelp:" <> tshow aiPid <> ":" <> tshow tmPid <> ":" <> tshow eid
    , kind = "offerHelp"
    , fromPlayer = aiPid
    , fromInvestigator = aiIid
    , fromName = fromName
    , toPlayer = tmPid
    , toInvestigator = tmIid
    , toName = tmName
    , toIsAi = toIsAi
    , prompt = fromName <> " can move in and help " <> tmName <> " fight " <> enemyName <> "."
    , options =
        [ AiQuestionOption
            { label = "Yes"
            , messages =
                [ AddAiPriority aiPid (EnemyTarget eid)
                , SetAiFocusOverride aiPid (Just CombatFocus)
                ]
            }
        , AiQuestionOption {label = "No", messages = []}
        ]
    , -- An AI teammate accepts help it needs (the enemy is tough for it); else
      -- declines. Human teammates decide for themselves (no auto-answer).
      aiAnswer = if toIsAi then Just (if toughForTm then 0 else 1) else Nothing
    }

{- | The most damage a single controlled weapon's tagged Fight ability deals per
attack ('abDamage'), floored at 1 (the unarmed base attack). Same fold as
@gatherSituation@'s @bestWeaponDamage@ in Decision.hs.
-}
bestWeaponDamage :: (HasGame m, Tracing m) => InvestigatorId -> m Int
bestWeaponDamage iid = do
  assetCards <- traverse (field AssetCard) =<< select (assetControlledBy iid)
  pure
    $ foldl'
      max
      1
      [ dmg
      | c <- assetCards
      , Just t <- [lookupCardTag (toCardCode c)]
      , ab <- Map.elems (ctAbilities t)
      , Just dmg <- [abDamage ab]
      ]

-- * Offer commit

{- | While a skill test is open, every ENABLED AI seat that is NOT the performer
and holds a card it could legally commit offers to commit its single best
(most-matching-icons) card to boost the performer's test. Read-only; produces no
questions when no skill test is open (or it has no performer). Shares the seat
enumeration with 'offerHelpForSeat'.
-}
offerCommitQuestions :: (HasGame m, Tracing m) => [PlayerId] -> m [AiQuestion]
offerCommitQuestions enabledAiSeats =
  getSkillTest >>= \case
    Nothing -> pure []
    Just _ ->
      getSkillTestInvestigator >>= \case
        Nothing -> pure []
        Just performerIid -> do
          performerPid <- field InvestigatorPlayerId performerIid
          performerName <- toTitle <$> field InvestigatorName performerIid
          performerIsAi <- maybe False aiEnabled <$> getAiPlayerState performerPid
          matching <- getSkillTestMatchingSkillIcons
          -- The performer's auto-answer is constant across asker seats: an AI
          -- performer accepts a boost (option 0) only when it isn't already
          -- comfortably passing — its current modified value is below
          -- difficulty + 1 — otherwise it declines (option 1). A human performer
          -- decides for itself (no auto-answer).
          autoAnswer <-
            if performerIsAi
              then do
                cur <- getSkillTestModifiedSkillValue
                mDiff <- getSkillTestDifficulty
                pure $ Just (if maybe True (\d -> cur < d + 1) mDiff then 0 else 1)
              else pure Nothing
          fmap concat
            $ for enabledAiSeats
            $ offerCommitForSeat performerIid performerPid performerName performerIsAi autoAnswer matching

{- | The offer-commit question a single AI seat (the asker) raises: at most one,
its best committable card. The performer never offers to commit to its own test,
so a seat controlling the performer is skipped. The engine already gates
'getCommittableCards' (co-located, matching\/wild icon for non-performers, etc.),
so any returned card is a legal, useful commit.
-}
offerCommitForSeat
  :: (HasGame m, Tracing m)
  => InvestigatorId
  -- ^ performer
  -> PlayerId
  -- ^ performer seat
  -> Text
  -- ^ performer name
  -> Bool
  -- ^ performer is an AI seat
  -> Maybe Int
  -- ^ performer auto-answer (option index, or Nothing for a human performer)
  -> Set SkillIcon
  -- ^ icons that count toward this test (includes WildIcon)
  -> PlayerId
  -- ^ asker seat
  -> m [AiQuestion]
offerCommitForSeat performerIid performerPid performerName performerIsAi autoAnswer matching aiPid =
  selectOne (InvestigatorIsPlayer aiPid) >>= \case
    Nothing -> pure []
    Just aiIid
      | aiIid == performerIid -> pure []
      | otherwise -> do
          committable <- getCommittableCards aiIid
          case bestCommit matching committable of
            Nothing -> pure []
            Just bestCard -> do
              askerName <- toTitle <$> field InvestigatorName aiIid
              pure
                [ mkOfferCommit
                    aiPid
                    aiIid
                    askerName
                    performerPid
                    performerIid
                    performerName
                    performerIsAi
                    autoAnswer
                    bestCard
                ]

{- | The committable card adding the most matching icons. Ties resolve to the
first (a later card only wins on a STRICTLY greater count), mirroring the
'matchIconValue' metric 'Arkham.Ai.Decision.decideAiAssist' uses. 'Nothing' for
an empty list — a left fold, never ClassyPrelude's partial list @maximum@.
-}
bestCommit :: Set SkillIcon -> [Card] -> Maybe Card
bestCommit matching = foldl' pick Nothing
 where
  pick Nothing c = Just c
  pick (Just best) c
    | matchIconValue matching c > matchIconValue matching best = Just c
    | otherwise = Just best

-- | How many of a card's printed icons count toward the current test (matching
-- skill icons and wilds), identical to 'Arkham.Ai.Decision.matchIconValue'.
matchIconValue :: Set SkillIcon -> Card -> Int
matchIconValue matching card =
  length (filter (`member` matching) (cdSkills (toCardDef card)))

{- | Build the offer-commit question. Accepting (option 0) commits the ASKER's
best card to the open skill test via 'SkillTestCommitCard' (a raw 'Message' that
commits the card while preserving the parked windows); declining (option 1) does
nothing. The resolution targets the asker's own commit — no 'AddAiPriority'.
-}
mkOfferCommit
  :: PlayerId
  -> InvestigatorId
  -> Text
  -> PlayerId
  -> InvestigatorId
  -> Text
  -> Bool
  -> Maybe Int
  -> Card
  -> AiQuestion
mkOfferCommit aiPid aiIid askerName performerPid performerIid performerName performerIsAi autoAnswer bestCard =
  AiQuestion
    { id = "offerCommit:" <> tshow aiPid <> ":" <> tshow performerPid <> ":" <> cardIdText
    , kind = "offerCommit"
    , fromPlayer = aiPid
    , fromInvestigator = aiIid
    , fromName = askerName
    , toPlayer = performerPid
    , toInvestigator = performerIid
    , toName = performerName
    , toIsAi = performerIsAi
    , prompt = askerName <> " can commit " <> cardName <> " to boost " <> performerName <> "'s test."
    , options =
        [ AiQuestionOption {label = "Yes", messages = [SkillTestCommitCard aiIid bestCard]}
        , AiQuestionOption {label = "No", messages = []}
        ]
    , aiAnswer = autoAnswer
    }
 where
  cardName = toTitle (toName bestCard)
  cardIdText = tshow (toCardId bestCard)
