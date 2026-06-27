{- |
Module      : Arkham.Ai.Decision
Description : The AI-investigator MVP decision engine.

Given a parked 'Question' for an AI-controlled seat, 'decideAi' always
produces a single legal 'Answer'. It never throws and never deadlocks on a
well-formed game snapshot: every branch resolves to a concrete answer, with a
safe fallback for question shapes we do not yet model.

== Index correctness (the load-bearing invariant)

For index-style questions the engine's answer worker (@Entity.Answer.go@)
indexes into the /flattened/ choice list /after/ peeling the
'QuestionLabel' \/ 'PayCostQuestion' \/ 'QuestionWithSource' wrappers. We
mirror that exactly:

  * wrappers are stripped by 'unwrapQuestion' before indexing;
  * 'ChooseOneFromEach' indexes into @concat groups@;
  * 'ChooseOneAtATimeWithAuto' reserves index @0@ for the auto-resolve-all
    choice (real choices are @+1@) — we always answer @0@ (auto);
  * every other index question indexes straight into its @choices@ list.

The returned 'QuestionResponse' carries @qrPlayerId = Just pid@ and
@qrQuestionVersion = Just gameScenarioSteps@ so the engine accepts it as
current rather than stale.

== Policy (v3 — weighted focus blend, deterministic, meant to be iterated)

We read the seat's situation (engaged enemies, modified stats, location
clues\/shroud, in-play enemy stats, priorities, the live skill test, and a
one-hop /move target/ toward the active objective or a priority) once from a
pure snapshot, compute a /focus weight blend/, then score each selectable
choice.

The blend ('aiFocusWeights', see 'focusWeightsFor') sums three signals per
'Focus':

  * /Specialty/ — the profile's hand-authored 'itWeights' plus each modified
    skill value folded onto the focus that skill tests (combat→'CombatFocus'
    etc.);
  * /Cards & abilities/ — every card the seat can act with now (current hand +
    controlled in-play assets) contributes its ai-tags 'ctWeight' to each
    focus it aligns to;
  * /Act objective/ — a flat +'actFocusBump' onto the focus the current act
    demands.

An 'aiFocusOverride' is a directive: its focus is slammed to a dominating
weight so it is the unconditional argmax. The /active focus/ ('aiActiveFocus',
used for movement\/objective routing) is the override if set, else
@argmax aiFocusWeights@. Scoring no longer uses a binary align bonus: a
choice's focus earns a proportional 'focusWeightBonus' (its blended weight
scaled into @0..'focusBonusCap'@ against the heaviest focus), so actions
aligned to a heavily-weighted focus score higher without eclipsing the kind
scores.

Four behaviours are load-bearing here:

  * /Skill-test commit window/ (the @StartSkillTestButton@ 'ChooseOne'). This
    window re-asks itself after every commit\/uncommit, so a naive scorer
    loops forever. We special-case it in 'chooseIndexFor' before generic
    scoring: if our committed total already clears @difficulty + 1@ (a one
    token hedge) we press /Start/; otherwise we commit the hand card that
    adds the most matching icons; we /never/ uncommit. Each commit raises the
    total and shrinks the choice list, so the loop terminates (and we press
    Start once over the line, or when nothing committable helps).

  * /Economy as setup/. Taking resources and drawing are valued by whether
    they advance the plan, not flat-zeroed. Taking resources scores 6 only
    when there is a concrete aligned hand card we cannot yet afford
    ('aiResourceShortfall' > 0); drawing scores 5 when we are missing our key
    focus card entirely ('aiShouldDig'), 2 for ordinary card advantage, and 0
    into a full hand. So the seat saves toward the card it wants or digs for
    its tools, but never burns every action farming resources it cannot spend,
    and a viable productive action (8+) always outranks economy (<= 6).

  * /Movement toward objectives/. We precompute a single one-hop
    'aiMoveTarget' — the next step toward clues (investigate focus) or toward
    the act's defeat-enemy target (combat focus). The basic Move action and
    the move-destination prompt both steer to it.

  * /Acting well/. Turn-menu basic actions are classified by their ability
    index (fight\/evade\/investigate\/move) and hand-card plays by their
    focus tag, so the seat fights killable enemies, investigates clue
    locations it can succeed at, moves toward objectives, and plays aligned
    weapons\/tools\/economy — each of which outscores resources, draw, and
    ending the turn.

Scoring is additive and intentionally simple — it is not chaos-bag math.
Fights prefer enemies we can finish (low remaining health, favourable to-hit
via @combat >= enemyFight@); evades fire when @agility >= enemyEvade@;
investigates fire only where there are clues and @intellect >= shroud@;
priority targets dominate; reaction\/fast abilities clear the "skip" baseline
so free value (e.g. an after-kill clue) is taken even off-focus; otherwise
the seat ends its turn rather than flailing.
-}
module Arkham.Ai.Decision (
  decideAi,
  isAssistCommitWindow,
  decideAiAssist,
  chooseIndexFor,
  scoreChoice,
  AiSituation (..),
  EnemyInfo (..),
  SkillTestInfo (..),
  emptySituation,
) where

import Arkham.Prelude

import Arkham.Ability (abilityActions, abilityIsFastAbility, abilityIsReactionAbility)
import Arkham.Ability.Types (Ability, abilityCardCode, abilityIndex, abilityTarget)
import Arkham.Act.Types (Field (ActCard))
import Arkham.Ai.Focus (Focus (..), allFoci, focusForAction, focusForSkill)
import Arkham.Ai.State (AiPlayerState (..))
import Arkham.Ai.Tags (
  ActObjective,
  InvestigatorTag,
  Objective (..),
  abFocuses,
  aoFocus,
  aoObjective,
  ctFocuses,
  ctWeight,
  itDeckFocus,
  itWeights,
  lookupAbilityTag,
  lookupActObjective,
  lookupCardTag,
  lookupInvestigatorTag,
 )
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Card (Card, printedCardCost)
import Arkham.Card.CardCode (toCardCode)
import Arkham.Card.CardDef (cdCardSubType, cdCardType, cdSkills, toCardDef)
import Arkham.Card.CardType (CardType (AssetType, EventType))
import Arkham.Classes.HasGame (HasGame, getGame)
import Arkham.Classes.Query (select, selectOne)
import Arkham.Constants (
  pattern AbilityAttack,
  pattern AbilityEngage,
  pattern AbilityEvade,
  pattern AbilityInvestigate,
  pattern AbilityMove,
 )
import Arkham.Distance (unDistance)
import Arkham.Enemy.Types (
  Field (EnemyEvade, EnemyFight, EnemyHealth, EnemyHealthDamage, EnemyLocation),
 )
import Arkham.Game ()
-- brings the orphan @Tracing Identity@ instance into scope
import Arkham.Game.Base (gameScenarioSteps)
import Arkham.GameEnv (getDistance, getSkillTest)
import Arkham.Helpers.Investigator (getHandSize, getMaybeLocation, modifiedStatsOf)
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Helpers.SkillTest (
  getSkillTestDifficulty,
  getSkillTestInvestigator,
  getSkillTestMatchingSkillIcons,
  getSkillTestModifiedSkillValue,
 )
import Arkham.Id (EnemyId, InvestigatorId, LocationId, PlayerId, ScenarioId (..))
import Arkham.Investigator.Types (
  Field (InvestigatorHand, InvestigatorRemainingActions, InvestigatorResources),
 )
import Arkham.Location.Types (Field (LocationClues, LocationShroud))
import Arkham.Matcher (
  assetControlledBy,
  enemyEngagedWith,
  enemyIs,
  pattern AnyAct,
  pattern AnyInPlayEnemy,
  pattern InvestigatorIsPlayer,
  pattern LocationWithAnyClues,
  pattern TheScenario,
 )
import Arkham.Matcher.Location (LocationMatcher (Anywhere, ClosestPathLocation, LocationWithId))
import Arkham.Message (
  Message,
  pattern InitiatePlayCardWithWindows,
  pattern SkillTestCommitCard,
  pattern SkillTestUncommitCard,
 )
import Arkham.Projection (field)
import Arkham.Question
import Arkham.SkillType (SkillIcon, SkillType (..))
import Arkham.Source (Source (GameSource))
import Arkham.Stats (Stats, statsSkillValue)
import Arkham.Target (Target (..))
import Arkham.Tracing (Tracing)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Entity.Answer (
  AmountsResponse (..),
  Answer (..),
  PaymentAmountsResponse (..),
  QuestionResponse (..),
 )

{- | The entry point other agents call. Always returns a legal 'Answer'.

NOTE — assist windows: 'decideAi' is the /auto-drive/ path and must never be
pointed at a non-performer skill-test commit window (see 'isAssistCommitWindow').
That window re-asks itself after every commit\/uncommit and carries no
@StartSkillTestButton@ \/ @Done@, so 'decideAi' would commit-then-uncommit
forever. We deliberately do /not/ special-case it here: the caller
(@Api.Handler.Arkham.Games.Shared.updateGame@) gates it out via
'isAssistCommitWindow' and declines (leaves the seat parked). On-demand single
commits go through 'decideAiAssist' instead. 'decideAi' is left unchanged.
-}
decideAi :: HasGame m => AiPlayerState -> PlayerId -> Question Message -> m Answer
decideAi state pid q = do
  g <- getGame
  -- The AI only inspects state, so we evaluate every read-only query against a
  -- pure snapshot in @ReaderT Game Identity@ (which is both 'HasGame' and
  -- 'Tracing'). That keeps 'decideAi''s constraint to just 'HasGame'.
  let sit = runIdentity (runReaderT (gatherSituation state pid) g)
  pure (buildAnswer sit pid (gameScenarioSteps g) q)

-- * Skill-test assisting (multiplayer / multihanded)

{- | True iff @pid@'s parked question is the /non-performer/ commit window of an
open skill test — i.e. another investigator is performing the test and this
seat has been handed a commit\/uncommit 'ChooseOne'.

All three conditions must hold:

  1. a skill test is open ('getSkillTestInvestigator' is @Just p@);
  2. this seat resolves to an investigator @iid@ with @p /= iid@ (we are an
     assister, not the performer);
  3. the unwrapped question's choices are /only/ commit\/uncommit choices
     (each choice's first message is 'SkillTestCommitCard' or
     'SkillTestUncommitCard') with no 'StartSkillTestButton'.

The performer's own window always contains a 'StartSkillTestButton', so (3)
excludes it. The caller uses this to (a) decline the auto-drive 'decideAi' on
these windows (which would loop) and (b) route 'AiAssist' triggers to
'decideAiAssist'.
-}
isAssistCommitWindow :: HasGame m => PlayerId -> Question Message -> m Bool
isAssistCommitWindow pid q0 = do
  g <- getGame
  -- 'selectOne' needs 'Tracing'; evaluate it against the pure snapshot exactly
  -- like 'decideAi' does, keeping the public constraint at 'HasGame'.
  let isAssistSeat = runIdentity (runReaderT (assistingSeat pid) g)
  pure (isAssistSeat && isCommitOnlyChoiceSet (unwrapQuestion q0))

{- | True iff a skill test is open and its performer is a /different/
investigator from @pid@'s seat.
-}
assistingSeat :: (HasGame n, Tracing n) => PlayerId -> n Bool
assistingSeat pid = do
  mPerformer <- getSkillTestInvestigator
  mSeat <- selectOne (InvestigatorIsPlayer pid)
  pure $ case (mPerformer, mSeat) of
    (Just p, Just iid) -> p /= iid
    _ -> False

{- | The non-performer commit window's choice shape: a non-empty list of only
commit\/uncommit choices and no 'StartSkillTestButton'.
-}
isCommitOnlyChoiceSet :: Question Message -> Bool
isCommitOnlyChoiceSet q = case flattenChoices q of
  Just (cs, _) ->
    not (null cs)
      && not (any isStartSkillTestButton cs)
      && all isCommitOrUncommitChoice cs
  Nothing -> False

{- | Whether a choice is a commit or uncommit choice (its first message is
'SkillTestCommitCard' \/ 'SkillTestUncommitCard' on a 'CardIdTarget').
-}
isCommitOrUncommitChoice :: UI Message -> Bool
isCommitOrUncommitChoice = \case
  TargetLabel (CardIdTarget _) (SkillTestCommitCard _ _ : _) -> True
  TargetLabel (CardIdTarget _) (SkillTestUncommitCard _ _ : _) -> True
  _ -> False

{- | On-demand single commit for an assisting seat. In the parked assist commit
window, pick the /commit/ choice whose card adds the most matching icons
(same v2 metric as the performer's commit window: card icons that count per
'getSkillTestMatchingSkillIcons'; ties resolve to the first). Returns the
'Answer' that selects that choice — applied through the normal answer path it
commits exactly one card, since the engine's one-commit-per-assister
restriction drops the re-asked window after a single commit.

Returns 'Nothing' when there is nothing to add (no commit choices — e.g. the
window only offers uncommits), in which case the caller treats it as a no-op
and leaves the seat parked.

The returned index is into the flattened, wrapper-stripped choice list (the
same invariant as 'decideAi'): for the assist 'ChooseOne' the offset is 0.
-}
decideAiAssist :: HasGame m => PlayerId -> Question Message -> m (Maybe Answer)
decideAiAssist pid q0 = do
  matching <- getSkillTestMatchingSkillIcons
  steps <- gameScenarioSteps <$> getGame
  pure $ case flattenChoices (unwrapQuestion q0) of
    Just (cs, off) ->
      let commits =
            [ (off + i, matchIconValue matching card)
            | (i, ui) <- withIndex cs
            , Just card <- [commitCardOf ui]
            ]
       in case maxesBy snd commits of
            ((i, _) : _) ->
              Just (Answer QuestionResponse {qrChoice = i, qrPlayerId = Just pid, qrQuestionVersion = Just steps})
            [] -> Nothing
    Nothing -> Nothing

-- * Situation

{- | Coarse, snapshot stats for a single in-play enemy. Remaining health is
@Nothing@ for enemies that cannot be defeated by damage.
-}
data EnemyInfo = EnemyInfo
  { eiRemainingHealth :: Maybe Int
  , eiFight :: Maybe Int
  , eiEvade :: Maybe Int
  }
  deriving stock Show

{- | A snapshot of the live skill test, captured only while one is open. Used
exclusively by the commit-window special case in 'chooseIndexFor'.
-}
data SkillTestInfo = SkillTestInfo
  { stiCurrent :: Int
  -- ^ our current committed total (base skill + committed icons), pre-token.
  , stiDifficulty :: Maybe Int
  -- ^ the modified difficulty, if known.
  , stiMatchingIcons :: Set SkillIcon
  -- ^ the icon set that counts toward this test (includes @WildIcon@).
  }
  deriving stock Show

-- | Everything the policy needs, read once per decision.
data AiSituation = AiSituation
  { aiState :: AiPlayerState
  , aiIid :: Maybe InvestigatorId
  , aiActiveFocus :: Focus
  {- ^ override if set, else @argmax 'aiFocusWeights'@; drives movement\/objective
  routing and economy alignment.
  -}
  , aiFocusWeights :: Map Focus Int
  {- ^ the blended strategic weight per focus (specialty + cards + act, or a maxed
  override). Drives the proportional 'focusWeightBonus' in 'scoreChoice'.
  -}
  , aiEngaged :: [EnemyId]
  , aiStats :: Maybe Stats
  , aiResources :: Int
  -- ^ plumbed for future "is drawing safe / do we have an action to spare" gating
  , aiRemainingActions :: Int
  -- ^ plumbed for future action-economy gating
  , aiLocationClues :: Int
  , aiLocationShroud :: Maybe Int
  , aiEnemies :: Map EnemyId EnemyInfo
  , aiPrioritySet :: [Target]
  , aiSkillTest :: Maybe SkillTestInfo
  -- ^ live skill-test snapshot; drives the commit-window special case.
  , aiMoveTarget :: Maybe LocationId
  -- ^ the immediate one-hop location to step to toward the active objective.
  , aiLocationDistances :: Map LocationId Int
  {- ^ distance from each location to the move objective (for the destination
  prompt tie-break); empty when there is no objective.
  -}
  , aiResourceShortfall :: Int
  {- ^ minimum resources still needed to afford an aligned, playable hand card we
  cannot yet pay for; 0 when none (or when an aligned card is already
  affordable, since we would just play it). Drives "take resources".
  -}
  , aiShouldDig :: Bool
  {- ^ we hold no aligned playable card and control no aligned asset, so drawing
  to find our key focus card is the best setup. Drives "draw".
  -}
  , aiHandNotFull :: Bool
  {- ^ hand is below the max-hand-size limit; gates drawing so we never draw into
  a discard.
  -}
  }
  deriving stock Show

{- | A situation with no seat resolved (used only when the player has no
investigator, e.g. mid-setup). Focus still comes from the configured profile.
-}
emptySituation :: AiPlayerState -> AiSituation
emptySituation state =
  AiSituation
    { aiState = state
    , aiIid = Nothing
    , aiActiveFocus = profileFocus state Nothing
    , aiFocusWeights = mempty
    , aiEngaged = []
    , aiStats = Nothing
    , aiResources = 0
    , aiRemainingActions = 0
    , aiLocationClues = 0
    , aiLocationShroud = Nothing
    , aiEnemies = mempty
    , aiPrioritySet = aiPriorities state
    , aiSkillTest = Nothing
    , aiMoveTarget = Nothing
    , aiLocationDistances = mempty
    , aiResourceShortfall = 0
    , aiShouldDig = False
    , aiHandNotFull = True
    }

gatherSituation :: (HasGame n, Tracing n) => AiPlayerState -> PlayerId -> n AiSituation
gatherSituation state pid = do
  mIid <- selectOne (InvestigatorIsPlayer pid)
  case mIid of
    Nothing -> pure (emptySituation state)
    Just iid -> do
      engaged <- select (enemyEngagedWith iid)
      stats <- modifiedStatsOf Nothing iid
      resources <- field InvestigatorResources iid
      remActions <- field InvestigatorRemainingActions iid
      mLoc <- getMaybeLocation iid
      (locClues, locShroud) <- case mLoc of
        Nothing -> pure (0, Nothing)
        Just lid -> do
          c <- field LocationClues lid
          s <- field LocationShroud lid
          pure (c, s)
      enemyIds <- select AnyInPlayEnemy
      enemyInfos <- for enemyIds $ \eid -> do
        h <- field EnemyHealth eid
        d <- field EnemyHealthDamage eid
        f <- field EnemyFight eid
        ev <- field EnemyEvade eid
        pure (eid, EnemyInfo (fmap (subtract d) h) f ev)
      -- Read hand + controlled in-play asset cards once; both the focus blend and
      -- the economy snapshot consume them, so we never re-query these fields.
      handCards <- field InvestigatorHand iid
      assetIds <- select (assetControlledBy iid)
      assetCards <- traverse (field AssetCard) assetIds
      mActObjective <- currentActObjective
      let
        mTag = lookupInvestigatorTag (aiInvestigatorCode state)
        focusWeights =
          focusWeightsFor (aiFocusOverride state) mTag (Just stats) handCards assetCards mActObjective
        focus = computeActiveFocus state focusWeights
      skillTestInfo <- gatherSkillTest
      (moveTarget, distances) <-
        gatherMovement
          iid
          focus
          (notNull engaged)
          mLoc
          locClues
          locShroud
          stats
          (aoObjective <$> mActObjective)
          (aiPriorities state)
      econ <- gatherEconomy iid focus resources handCards assetCards
      pure
        AiSituation
          { aiState = state
          , aiIid = Just iid
          , aiActiveFocus = focus
          , aiFocusWeights = focusWeights
          , aiEngaged = engaged
          , aiStats = Just stats
          , aiResources = resources
          , aiRemainingActions = remActions
          , aiLocationClues = locClues
          , aiLocationShroud = locShroud
          , aiEnemies = Map.fromList enemyInfos
          , aiPrioritySet = aiPriorities state
          , aiSkillTest = skillTestInfo
          , aiMoveTarget = moveTarget
          , aiLocationDistances = distances
          , aiResourceShortfall = ecResourceShortfall econ
          , aiShouldDig = ecShouldDig econ
          , aiHandNotFull = ecHandNotFull econ
          }

-- * Active focus

{- | The active focus used for movement\/objective routing and economy
alignment: 'aiFocusOverride' wins (a directive); otherwise the @argmax@ of the
blended 'aiFocusWeights'. Falls back to the profile's dominant focus only when
the blend is empty (no profile and no stats, e.g. 'emptySituation').
-}
computeActiveFocus :: AiPlayerState -> Map Focus Int -> Focus
computeActiveFocus state weights =
  fromMaybe (profileFocus state Nothing) (aiFocusOverride state <|> argmaxFocus weights)

{- | The single heaviest focus in a weight blend (ties resolve to the lowest
'Focus' constructor, keeping the pick deterministic), or 'Nothing' when no
focus carries positive weight.
-}
argmaxFocus :: Map Focus Int -> Maybe Focus
argmaxFocus weights = case maxesBy snd [(f, w) | (f, w) <- Map.toList weights, w > 0] of
  ((f, _) : _) -> Just f
  [] -> Nothing

{- | Blend the seat's strategic focus weights from three signals (raw, additive;
the @0..'focusBonusCap'@ normalization for scoring happens later in
'focusWeightBonus'):

  * /Specialty/ — the profile's hand-authored 'itWeights' (≈1–4 each) plus
    each modified skill value (≈1–6) folded onto the focus that skill tests
    ('focusForSkill': combat→'CombatFocus', intellect→'InvestigateFocus',
    agility→'EvadeFocus', willpower→'SurvivalFocus'). Skill values are the
    dominant raw term; 'itWeights' nudge.
  * /Cards & abilities/ — every card the seat can act with right now (its
    current hand plus controlled in-play assets) adds its ai-tags 'ctWeight'
    (0–4) to each focus it is aligned to, so a stocked toolkit pulls its focus
    up by several points.
  * /Act objective/ — a flat +'actFocusBump' onto the focus the current act
    demands, tilting the blend toward the table's goal without steamrolling a
    strong specialty.

An 'aiFocusOverride' is a directive: we slam its focus to 'overrideWeight' so
it dominates every blended focus (the unconditional argmax, earning the full
proportional bonus while the rest collapse toward zero), leaving the other
entries intact for reference.
-}
focusWeightsFor
  :: Maybe Focus
  -> Maybe InvestigatorTag
  -> Maybe Stats
  -> [Card]
  -> [Card]
  -> Maybe ActObjective
  -> Map Focus Int
focusWeightsFor mOverride mTag mStats handCards assetCards mActObjective =
  case mOverride of
    Just f -> Map.insert f overrideWeight blended
    Nothing -> blended
 where
  blended = Map.fromListWith (+) (specialty <> cardWeights <> act)
  specialty = tagWeights <> skillWeights
  tagWeights = maybe [] (Map.toList . itWeights) mTag
  skillWeights = case mStats of
    Nothing -> []
    Just stats ->
      [ (focusForSkill s, statsSkillValue stats s)
      | s <- [SkillWillpower, SkillIntellect, SkillCombat, SkillAgility]
      ]
  cardWeights =
    [ (f, ctWeight tag)
    | c <- handCards <> assetCards
    , Just tag <- [lookupCardTag (toCardCode c)]
    , f <- ctFocuses tag
    ]
  act = maybe [] (\ao -> [(aoFocus ao, actFocusBump)]) mActObjective

-- | The flat weight the current act's objective focus contributes to the blend.
actFocusBump :: Int
actFocusBump = 3

{- | The dominating weight an 'aiFocusOverride' focus is forced to. Far above any
realistic blended total (specialty + a full toolkit + act ≈ low-30s) so the
override is always the argmax and earns the full 'focusBonusCap'.
-}
overrideWeight :: Int
overrideWeight = 1000

{- | The current act's strategic objective (focus + concrete goal), if the
scenario\/act is tagged. Read once and reused for both focus and movement.
-}
currentActObjective :: (HasGame n, Tracing n) => n (Maybe ActObjective)
currentActObjective = do
  mScenario <- selectOne TheScenario
  mAct <- selectOne AnyAct
  case (mScenario, mAct) of
    (Just (ScenarioId scc), Just aid) -> do
      card <- field ActCard aid
      pure (lookupActObjective scc (toCardCode card))
    _ -> pure Nothing

-- * Skill test snapshot

{- | Capture the live skill test, or 'Nothing' when none is open. We guard the
@getJustSkillTest@-based reads behind 'getSkillTest' so they never error.
-}
gatherSkillTest :: (HasGame n, Tracing n) => n (Maybe SkillTestInfo)
gatherSkillTest =
  getSkillTest >>= \case
    Nothing -> pure Nothing
    Just _ -> do
      diff <- getSkillTestDifficulty
      cur <- getSkillTestModifiedSkillValue
      icons <- getSkillTestMatchingSkillIcons
      pure (Just (SkillTestInfo {stiCurrent = cur, stiDifficulty = diff, stiMatchingIcons = icons}))

-- * Movement targeting

{- | Compute the immediate one-hop 'aiMoveTarget' and the distance map used to
break ties in the move-destination prompt. We never move while engaged (we
fight in place). Otherwise a user-set /priority/ location\/enemy reroutes
movement ahead of everything else; absent a priority we route toward clues
(investigate focus) or the act's defeat-enemy target (combat focus). Every
read is guarded so a missing location\/enemy just yields 'Nothing'.
-}
gatherMovement
  :: (HasGame n, Tracing n)
  => InvestigatorId
  -> Focus
  -> Bool
  -> Maybe LocationId
  -> Int
  -> Maybe Int
  -> Stats
  -> Maybe Objective
  -> [Target]
  -> n (Maybe LocationId, Map LocationId Int)
gatherMovement iid focus engaged mHere locClues mShroud stats mObjective priorities
  | engaged = pure (Nothing, mempty)
  | otherwise = do
      mPriorityDest <- priorityDestination
      (mStep, mDest) <- case mPriorityDest of
        Just dest -> routeToward dest
        Nothing -> case focus of
          InvestigateFocus
            | worthInvestigatingHere -> pure (Nothing, Nothing)
            | otherwise -> investigateRoute
          CombatFocus -> case mObjective of
            Just (DefeatEnemyObjective cc) -> enemyRoute cc
            _ -> pure (Nothing, Nothing)
          _ -> pure (Nothing, Nothing)
      distances <- maybe (pure mempty) distancesTo mDest
      pure (mStep, distances)
 where
  worthInvestigatingHere =
    locClues > 0 && maybe True (statsSkillValue stats SkillIntellect >=) mShroud

  -- A user-set priority reroutes movement ahead of any act-objective routing:
  -- the first prioritized location, else the first prioritized enemy's current
  -- location. So a distant "go here" actually walks there.
  priorityDestination = case mapMaybe (.location) priorities of
    (lid : _) -> pure (Just lid)
    [] -> case mapMaybe (.enemy) priorities of
      (eid : _) -> field EnemyLocation eid
      [] -> pure Nothing

  -- One-hop step toward @dest@ plus the destination itself (for the distance
  -- map): 'Nothing' when we are already there or no path is known. Shared by the
  -- priority and defeat-enemy routes.
  routeToward dest = case mHere of
    Nothing -> pure (Nothing, Just dest)
    Just here
      | here == dest -> pure (Nothing, Nothing)
      | otherwise -> do
          oneHop <- getCanMoveToMatchingLocations iid GameSource (LocationWithId dest)
          if dest `elem` oneHop
            then pure (Just dest, Just dest)
            else do
              mStep <- selectOne (ClosestPathLocation here dest)
              pure (mStep, Just dest)

  -- Investigate: step onto any adjacent clue location; else route toward the
  -- nearest clue location.
  investigateRoute = do
    oneHop <- getCanMoveToMatchingLocations iid GameSource LocationWithAnyClues
    case oneHop of
      (lid : _) -> pure (Just lid, Just lid)
      [] -> case mHere of
        Nothing -> pure (Nothing, Nothing)
        Just here -> do
          clueLocs <- select LocationWithAnyClues
          nearestLocation here clueLocs >>= \case
            Nothing -> pure (Nothing, Nothing)
            Just dest -> do
              mStep <- selectOne (ClosestPathLocation here dest)
              pure (mStep, Just dest)

  -- Defeat-enemy: walk toward the objective enemy's location.
  enemyRoute cc =
    selectOne (enemyIs cc) >>= \case
      Nothing -> pure (Nothing, Nothing)
      Just eid ->
        field EnemyLocation eid >>= \case
          Just dest -> routeToward dest
          Nothing -> pure (Nothing, Nothing)

  -- Nearest of @locs@ to @here@ by graph distance (excluding @here@).
  nearestLocation here locs = do
    scored <- for (filter (/= here) locs) $ \l -> do
      md <- getDistance here l
      pure (l, md)
    pure $ case sortOn snd [(l, unDistance d) | (l, Just d) <- scored] of
      ((l, _) : _) -> Just l
      [] -> Nothing

  -- Distance from every location to the chosen destination.
  distancesTo dest = do
    locs <- select Anywhere
    fmap Map.fromList $ for locs $ \l -> do
      md <- getDistance l dest
      pure (l, maybe farAway unDistance md)

{- | A sentinel distance for unreachable locations (kept finite so scoring math
stays in 'Int').
-}
farAway :: Int
farAway = 999

-- * Economy (setup) snapshot

{- | The economy facts scoring needs: the shortfall toward an aligned card we
want, whether we should dig for a missing focus card, and whether the hand has
room to draw.
-}
data EconomyInfo = EconomyInfo
  { ecResourceShortfall :: Int
  , ecShouldDig :: Bool
  , ecHandNotFull :: Bool
  }

{- | Derive the economy facts relative to the active focus from the
already-read hand + controlled asset cards and the hand-size limit. Economy
actions are setup: their value is whether they advance the plan (afford / find
the focus card), not a flat constant.
-}
gatherEconomy :: HasGame n => InvestigatorId -> Focus -> Int -> [Card] -> [Card] -> n EconomyInfo
gatherEconomy iid active resources handCards assetCards = do
  handLimit <- getHandSize iid
  let
    isAligned c = maybe False (elem active . ctFocuses) (lookupCardTag (toCardCode c))
    isPlayableSetup c =
      let def = toCardDef c
       in cdCardType def `elem` [AssetType, EventType] && isNothing (cdCardSubType def)
    alignedHand = filter (\c -> isAligned c && isPlayableSetup c) handCards
    hasAffordableAligned = any ((<= resources) . printedCardCost) alignedHand
    shortfall
      | hasAffordableAligned = 0
      | otherwise = case [printedCardCost c - resources | c <- alignedHand, printedCardCost c > resources] of
          [] -> 0
          (x : xs) -> foldl' min x xs
    hasAlignedInPlay = any isAligned assetCards
  pure
    EconomyInfo
      { ecResourceShortfall = shortfall
      , ecShouldDig = null alignedHand && not hasAlignedInPlay
      , ecHandNotFull = length handCards < handLimit
      }

{- | The investigator profile's dominant focus: tag weights (seeded by the
highest skill), falling back to the deck focus, then to the strongest stat.
-}
profileFocus :: AiPlayerState -> Maybe Stats -> Focus
profileFocus state mStats =
  case lookupInvestigatorTag (aiInvestigatorCode state) of
    Just tag -> dominantFocus (itWeights tag) (itDeckFocus tag) (statFocus <$> mStats)
    Nothing -> maybe SupportFocus statFocus mStats

dominantFocus :: Map Focus Int -> Focus -> Maybe Focus -> Focus
dominantFocus weights deckFocus mStatFocus
  | Map.null weights = deckFocus
  | otherwise = case maxesBy snd scored of
      ((f, _) : _) -> f
      [] -> deckFocus
 where
  seedFor f = Map.findWithDefault 0 f weights + (if Just f == mStatFocus then 1 else 0)
  scored = [(f, seedFor f) | f <- allFoci, seedFor f > 0]

statFocus :: Stats -> Focus
statFocus stats = case maxesBy snd skillPairs of
  ((f, _) : _) -> f
  [] -> SupportFocus
 where
  skillPairs =
    [ (focusForSkill s, statsSkillValue stats s)
    | s <- [SkillWillpower, SkillIntellect, SkillCombat, SkillAgility]
    ]

-- * Answer construction

buildAnswer :: AiSituation -> PlayerId -> Int -> Question Message -> Answer
buildAnswer sit pid steps q0 = case unwrapQuestion q0 of
  ChooseAmounts _ tgt choices _ ->
    AmountsAnswer
      AmountsResponse
        { arAmounts = distributeAmounts tgt (amountBounds choices)
        , arQuestionVersion = Just steps
        , arPlayerId = Just pid
        }
  ChoosePaymentAmounts _ mtgt choices ->
    PaymentAmountsAnswer
      PaymentAmountsResponse
        { parAmounts = distributeAmounts (fromMaybe (MaxAmountTarget 0) mtgt) (paymentBounds choices)
        , parQuestionVersion = Just steps
        , parPlayerId = Just pid
        }
  Read _ rc _ -> idx (readChoiceIndex rc)
  -- Everything else is an index question (or an out-of-scope shape, for which
  -- 'chooseIndexFor' returns Nothing and we answer index 0 defensively).
  q -> idx (fromMaybe 0 (chooseIndexFor sit q))
 where
  idx i = Answer QuestionResponse {qrChoice = i, qrPlayerId = Just pid, qrQuestionVersion = Just steps}
  amountBounds cs = [(c.choiceId, c.minBound, c.maxBound) | c <- cs]
  paymentBounds cs = [(c.choiceId, c.minBound, c.maxBound) | c <- cs]

{- | The engine index to answer for an index-style question, or 'Nothing' for a
shape that needs a non-index 'Answer' (or that we do not model). Exposed for
unit testing against synthetic 'Question's.
-}
chooseIndexFor :: AiSituation -> Question Message -> Maybe Int
chooseIndexFor sit q0 = commitWindowDecision sit q <|> go q
 where
  q = unwrapQuestion q0
  go = \case
    -- index 0 is the auto "resolve all in any order" choice and is always legal
    ChooseOneAtATimeWithAuto _ _ -> Just 0
    q' -> case flattenChoices q' of
      Just (cs, off) -> Just (off + pickFrom q' cs)
      Nothing -> Nothing
  pickFrom q' cs
    -- Batch (multi-select) windows: stay conservative. Take the explicit
    -- stop/skip when offered (don't waste cards/resources); otherwise the
    -- window is mandatory, so pick the first selectable choice.
    | isBatch q' = fromMaybe (firstSelectable cs) (firstStop cs)
    | otherwise = bestByScore sit cs

-- * Skill-test commit window (the hang fix)

{- | One token of hedge against the chaos bag: we only press /Start/ once our
committed total is at least @difficulty + 'commitBuffer'@.
-}
commitBuffer :: Int
commitBuffer = 1

{- | If @q@ is the skill-test commit window (a 'ChooseOne' containing a
'StartSkillTestButton'), decide between committing and starting; otherwise
'Nothing' so generic scoring runs. The returned index is into the
'ChooseOne' choices (offset 0), preserving the index invariant.
-}
commitWindowDecision :: AiSituation -> Question Message -> Maybe Int
commitWindowDecision sit = \case
  ChooseOne cs | any isStartSkillTestButton cs -> Just (decideCommit (aiSkillTest sit) cs)
  _ -> Nothing

{- | Commit the highest-icon hand card until we clear @difficulty + buffer@,
then press Start. Never uncommits. Terminates because every commit either
raises the committed total or, once nothing committable adds a matching icon,
falls through to Start (and the choice list shrinks with each commit anyway).
-}
decideCommit :: Maybe SkillTestInfo -> [UI Message] -> Int
decideCommit mSti cs = case mSti of
  Nothing -> startIdx
  Just sti -> case stiDifficulty sti of
    Nothing -> startIdx
    Just d
      | stiCurrent sti >= d + commitBuffer -> startIdx
      | otherwise ->
          let commits =
                [ (i, matchIconValue (stiMatchingIcons sti) card)
                | (i, ui) <- withIndex cs
                , Just card <- [commitCardOf ui]
                ]
           in case maxesBy snd commits of
                ((i, v) : _) | v > 0 -> i
                -- nothing committable helps the total: take the test as-is.
                _ -> startIdx
 where
  startIdx = fromMaybe 0 (listToMaybe [i | (i, ui) <- withIndex cs, isStartSkillTestButton ui])

isStartSkillTestButton :: UI msg -> Bool
isStartSkillTestButton = \case
  StartSkillTestButton _ -> True
  _ -> False

{- | The card a /commit/ choice would commit, or 'Nothing'. Distinguishes commit
from uncommit by the first message: an uncommit's first message is
'SkillTestUncommitCard', which does not match this 'SkillTestCommitCard'
pattern, so uncommits are never selected here.
-}
commitCardOf :: UI Message -> Maybe Card
commitCardOf = \case
  TargetLabel (CardIdTarget _) (SkillTestCommitCard _ card : _) -> Just card
  _ -> Nothing

{- | How many of a card's printed icons count toward the current test (matching
skill icons and wilds, per 'getSkillTestMatchingSkillIcons').
-}
matchIconValue :: Set SkillIcon -> Card -> Int
matchIconValue matching card =
  length (filter (`Set.member` matching) (cdSkills (toCardDef card)))

-- | Strip the wrappers the engine peels before indexing.
unwrapQuestion :: Question msg -> Question msg
unwrapQuestion = \case
  QuestionLabel _ _ q -> unwrapQuestion q
  PayCostQuestion _ q -> unwrapQuestion q
  QuestionWithSource _ _ q -> unwrapQuestion q
  q -> q

{- | The flattened, engine-indexed choice list and the index offset for a
question (offset is non-zero only for shapes that reserve leading indices).
-}
flattenChoices :: Question msg -> Maybe ([UI msg], Int)
flattenChoices = \case
  ChooseOne cs -> Just (cs, 0)
  PlayerWindowChooseOne cs -> Just (cs, 0)
  ChooseN _ cs -> Just (cs, 0)
  ChooseSome cs -> Just (cs, 0)
  ChooseSome1 _ cs -> Just (cs, 0)
  ChooseUpToN _ cs -> Just (cs, 0)
  ChooseOneAtATime cs -> Just (cs, 0)
  ChooseOneFromEach gs -> Just (concat gs, 0)
  PickSupplies _ _ cs _ -> Just (cs, 0)
  _ -> Nothing

isBatch :: Question msg -> Bool
isBatch = \case
  ChooseN {} -> True
  ChooseSome {} -> True
  ChooseSome1 {} -> True
  ChooseUpToN {} -> True
  _ -> False

readChoiceIndex :: ReadChoices Message -> Int
readChoiceIndex = \case
  BasicReadChoices cs -> firstSelectable cs
  BasicReadChoicesN _ cs -> firstSelectable cs
  BasicReadChoicesUpToN _ cs -> firstSelectable cs
  LeadInvestigatorMustDecide cs -> firstSelectable cs

firstSelectable :: [UI Message] -> Int
firstSelectable cs = maybe 0 fst (listToMaybe [(i, ui) | (i, ui) <- withIndex cs, isSelectable ui])

firstStop :: [UI Message] -> Maybe Int
firstStop cs = listToMaybe [i | (i, ui) <- withIndex cs, uiIsStop ui]

bestByScore :: AiSituation -> [UI Message] -> Int
bestByScore sit cs = case maxesBy snd scored of
  ((i, _) : _) -> i
  [] -> 0
 where
  scored = [(i, scoreChoice sit ui) | (i, ui) <- withIndex cs, isSelectable ui]

-- * Choice scoring

{- | A user-set priority is a directive: a choice whose target is in the seat's
'aiPrioritySet' gets this flat bonus so it dominates every focus-weighted
action. The heaviest /non-priority/ action tops out around the low-40s (an
objective move that is also the one-hop target: base 12 + focus cap 8 +
location 20; a finisher fight reaches ~30), so 100 makes any legal prioritized
choice win outright while still stacking its own kind score to order multiple
choices on the same target. It is purely additive over 'isSelectable' choices,
so it never resurrects an illegal action nor overrides the always-legal
fallback — a prioritized action we literally cannot take simply is not in the
scored list.
-}
priorityBonus :: Int
priorityBonus = 100

{- | The largest proportional focus bonus a fully-weighted focus earns. Chosen so
an aligned action's focus term (≤ this cap) sits in the same band as the kind
scores (fight\/evade\/investigate\/move bases are 2–12): it tilts ties toward
the seat's strategy without, on its own, eclipsing a killable-enemy finisher
or an objective move.
-}
focusBonusCap :: Int
focusBonusCap = 8

{- | The proportional focus term used in 'scoreChoice': scale a focus's blended
weight into @0..'focusBonusCap'@ against the heaviest focus in the blend. The
argmax focus earns the full cap; a half-weighted focus earns half; an
unweighted focus earns nothing. An override focus (weight 'overrideWeight')
earns the full cap while every other focus collapses toward zero.
-}
focusWeightBonus :: Map Focus Int -> Focus -> Int
focusWeightBonus weights f =
  let w = max 0 (Map.findWithDefault 0 f weights)
      maxW = foldl' max 1 (Map.elems weights)
   in (w * focusBonusCap) `div` maxW

{- | Score a single choice for the current situation; higher is more attractive.
Pure and deterministic so it can be unit-tested with synthetic choices. The
weights are coarse v1 heuristics to be tuned with play data, not a solved
policy.
-}
scoreChoice :: AiSituation -> UI Message -> Int
scoreChoice sit ui = priorityScore + kindScore + stopScore + locationScore + economyScore
 where
  skill s = (\st -> statsSkillValue st s) <$> aiStats sit
  -- Proportional focus term (replaces the old binary align bonus): a choice's
  -- focus earns up to 'focusBonusCap', scaled by how heavily that focus is
  -- weighted in the blend.
  focusBonus = focusWeightBonus (aiFocusWeights sit)

  priorityScore = case uiTarget ui of
    Just t | t `elem` aiPrioritySet sit -> priorityBonus
    _ -> 0

  -- Economy as setup. Taking resources is worth saving for only when there is a
  -- concrete aligned card we cannot yet afford; drawing digs for a missing focus
  -- card (5), is mild card advantage otherwise (2), and is never worth filling
  -- the hand into a discard (0). Both stay <= 6 so any viable productive action
  -- (8+) outranks them, and both beat ending the turn (1) when they make
  -- progress.
  economyScore = case ui of
    ResourceLabel _ _ -> if aiResourceShortfall sit > 0 then 6 else 0
    ComponentLabel (InvestigatorDeckComponent _) _
      | not (aiHandNotFull sit) -> 0
      | aiShouldDig sit -> 5
      | otherwise -> 2
    _ -> 0

  -- "Stop" baselines: skipping a reaction is neutral-ish (5); ending the turn
  -- is the lowest-value stop (1) so any worthwhile action outscores it.
  stopScore = case ui of
    EndTurnButton {} -> 1
    Done {} -> 5
    SkipTriggersButton {} -> 5
    _ -> 0

  -- Steer the move-destination prompt: the precomputed one-hop target wins
  -- outright; otherwise prefer the location closest to the objective. Scores 0
  -- for non-location targets and when there is no objective.
  locationScore = case uiTarget ui of
    Just (LocationTarget lid)
      | Just lid == aiMoveTarget sit -> 20
      | otherwise -> case Map.lookup lid (aiLocationDistances sit) of
          Just d -> max 0 (15 - d)
          Nothing -> 0
    _ -> 0

  kindScore = case classifyUI ui of
    FightChoice meid -> 4 + focusBonus CombatFocus + maybe 0 combatBonus meid
    EvadeChoice meid -> 2 + focusBonus EvadeFocus + maybe 0 evadeBonus meid
    InvestigateChoice -> case investigateBonus of
      0 -> 0
      n -> n + focusBonus InvestigateFocus
    MoveChoice -> case aiMoveTarget sit of
      Just _ -> 12 + focusBonus MobilityFocus
      Nothing -> 0
    PlayCardChoice mf -> case mf of
      Just f -> 8 + focusBonus f
      Nothing -> 2
    FocusedChoice f -> 6 + focusBonus f
    PlainChoice -> 0

  combatBonus eid = fromMaybe 0 $ do
    info <- Map.lookup eid (aiEnemies sit)
    let toHit = case (skill SkillCombat, eiFight info) of
          (Just c, Just f)
            | c >= f -> 8
            | c + 1 >= f -> 4
            | otherwise -> 0
          _ -> 0
        finish = case eiRemainingHealth info of
          Just rh
            | rh <= 1 -> 10
            | rh <= 3 -> 5
            | otherwise -> 0
          Nothing -> 0
    pure (toHit + finish)

  evadeBonus eid = fromMaybe 0 $ do
    info <- Map.lookup eid (aiEnemies sit)
    case (skill SkillAgility, eiEvade info) of
      (Just a, Just e) | a >= e -> pure 6
      _ -> pure 0

  -- Only an action-investigate is gated on there being clues here; reaction
  -- abilities that grant clues are handled as 'FocusedChoice' (see classifyUI).
  investigateBonus
    | aiLocationClues sit <= 0 = 0
    | otherwise = case (skill SkillIntellect, aiLocationShroud sit) of
        (Just i, Just sh)
          | i >= sh -> 8
          | otherwise -> 3
        _ -> 4

data ChoiceKind
  = FightChoice (Maybe EnemyId)
  | EvadeChoice (Maybe EnemyId)
  | InvestigateChoice
  | MoveChoice
  | PlayCardChoice (Maybe Focus)
  | FocusedChoice Focus
  | PlainChoice

classifyUI :: UI Message -> ChoiceKind
classifyUI = \case
  FightLabel eid _ -> FightChoice (Just eid)
  FightLabelWithSkill eid _ _ -> FightChoice (Just eid)
  EvadeLabel eid _ -> EvadeChoice (Just eid)
  EvadeLabelWithSkill eid _ _ -> EvadeChoice (Just eid)
  EngageLabel eid _ -> FightChoice (Just eid)
  ui@(AbilityLabel _ ab _ _ _) -> classifyAbility ui ab
  -- Hand-card plays in the turn menu are TargetLabels carrying an
  -- 'InitiatePlayCardWithWindows'; score them by the card's focus tag so the
  -- seat plays aligned weapons/tools/economy (CardLabel never appears here).
  TargetLabel (CardIdTarget _) (InitiatePlayCardWithWindows _ card _ _ _ _ : _) ->
    PlayCardChoice (cardFocus card)
  ui -> fromFocusAndEnemy (uiFocus ui) (uiEnemyId ui)

{- | Classify a turn-menu ability. Basic actions are distinguished by index;
reactions/fast abilities are free triggers (focused value); everything else
falls back to the ability's focus tag.
-}
classifyAbility :: UI Message -> Ability -> ChoiceKind
classifyAbility ui ab
  | abilityIsReactionAbility ab || abilityIsFastAbility ab =
      maybe PlainChoice FocusedChoice (abilityFocus ab)
  | idx == AbilityAttack = FightChoice (enemyFromAbility ab)
  | idx == AbilityEvade = EvadeChoice (enemyFromAbility ab)
  | idx == AbilityEngage = FocusedChoice CombatFocus
  | idx == AbilityInvestigate = InvestigateChoice
  | idx == AbilityMove = MoveChoice
  | otherwise = fromFocusAndEnemy (abilityFocus ab) (uiEnemyId ui)
 where
  idx = abilityIndex ab

enemyFromAbility :: Ability -> Maybe EnemyId
enemyFromAbility ab = case abilityTarget ab of
  Just (EnemyTarget eid) -> Just eid
  _ -> Nothing

-- | The card's primary focus from its tag profile, if tagged.
cardFocus :: Card -> Maybe Focus
cardFocus card = lookupCardTag (toCardCode card) >>= headMay . ctFocuses

fromFocusAndEnemy :: Maybe Focus -> Maybe EnemyId -> ChoiceKind
fromFocusAndEnemy mf me = case (mf, me) of
  (Just CombatFocus, Just eid) -> FightChoice (Just eid)
  (Just EvadeFocus, Just eid) -> EvadeChoice (Just eid)
  (Just InvestigateFocus, _) -> InvestigateChoice
  (Just f, _) -> FocusedChoice f
  (Nothing, _) -> PlainChoice

-- * UI projections

isSelectable :: UI msg -> Bool
isSelectable = \case
  Info _ -> False
  InvalidLabel _ -> False
  _ -> True

uiIsStop :: UI msg -> Bool
uiIsStop = \case
  Done _ -> True
  SkipTriggersButton _ -> True
  EndTurnButton _ _ -> True
  _ -> False

uiTarget :: UI Message -> Maybe Target
uiTarget = \case
  TargetLabel t _ -> Just t
  FightLabel eid _ -> Just (EnemyTarget eid)
  FightLabelWithSkill eid _ _ -> Just (EnemyTarget eid)
  EvadeLabel eid _ -> Just (EnemyTarget eid)
  EvadeLabelWithSkill eid _ _ -> Just (EnemyTarget eid)
  EngageLabel eid _ -> Just (EnemyTarget eid)
  AbilityLabel _ ab _ _ _ -> abilityTarget ab
  _ -> Nothing

uiEnemyId :: UI Message -> Maybe EnemyId
uiEnemyId = \case
  FightLabel eid _ -> Just eid
  FightLabelWithSkill eid _ _ -> Just eid
  EvadeLabel eid _ -> Just eid
  EvadeLabelWithSkill eid _ _ -> Just eid
  EngageLabel eid _ -> Just eid
  TargetLabel (EnemyTarget eid) _ -> Just eid
  AbilityLabel _ ab _ _ _ -> case abilityTarget ab of
    Just (EnemyTarget eid) -> Just eid
    _ -> Nothing
  _ -> Nothing

uiFocus :: UI Message -> Maybe Focus
uiFocus = \case
  FightLabel {} -> Just CombatFocus
  FightLabelWithSkill {} -> Just CombatFocus
  EvadeLabel {} -> Just EvadeFocus
  EvadeLabelWithSkill {} -> Just EvadeFocus
  EngageLabel {} -> Just CombatFocus
  SkillLabel s _ -> Just (focusForSkill s)
  SkillLabelWithLabel _ s _ -> Just (focusForSkill s)
  CardLabel cc _ _ -> lookupCardTag cc >>= headMay . ctFocuses
  AbilityLabel _ ab _ _ _ -> abilityFocus ab
  ComponentLabel comp _ -> componentFocus comp
  _ -> Nothing

{- | Components carry no intrinsic focus. Resources and draws are scored as
setup by 'economyScore' (see 'scoreChoice'), not via a focus tag — so they map
to 'Nothing' here and contribute 0 through 'kindScore'.
-}
componentFocus :: Component -> Maybe Focus
componentFocus = \case
  InvestigatorDeckComponent _ -> Nothing
  InvestigatorComponent _ ResourceToken -> Nothing
  _ -> Nothing

abilityFocus :: Ability -> Maybe Focus
abilityFocus ab =
  (lookupAbilityTag (abilityCardCode ab) (abilityIndex ab) >>= headMay . abFocuses)
    <|> listToMaybe (mapMaybe focusForAction (abilityActions ab))
    <|> (lookupCardTag (abilityCardCode ab) >>= headMay . ctFocuses)

-- * Amount fallbacks (deep fallback shapes)

{- | Build a minimal, legal amount distribution. For total\/min\/one-of targets
we greedily fill choices up to the target within their bounds; for a max
target (or none) we give each choice its lower bound.
-}
distributeAmounts :: AmountTarget -> [(UUID, Int, Int)] -> Map UUID Int
distributeAmounts tgt bounds = Map.fromList (go target bounds)
 where
  target = case tgt of
    TotalAmountTarget n -> n
    MinAmountTarget n -> n
    AmountOneOf (n : _) -> n
    AmountOneOf [] -> 0
    MaxAmountTarget _ -> 0
  go _ [] = []
  go remaining ((cid, lo, hi) : rest) =
    let give = max lo (min hi (max 0 remaining))
     in (cid, give) : go (remaining - give) rest
