-- |
-- Module      : Arkham.Ai.Decision
-- Description : The AI-investigator MVP decision engine.
--
-- Given a parked 'Question' for an AI-controlled seat, 'decideAi' always
-- produces a single legal 'Answer'. It never throws and never deadlocks on a
-- well-formed game snapshot: every branch resolves to a concrete answer, with a
-- safe fallback for question shapes we do not yet model.
--
-- == Index correctness (the load-bearing invariant)
--
-- For index-style questions the engine's answer worker (@Entity.Answer.go@)
-- indexes into the /flattened/ choice list /after/ peeling the
-- 'QuestionLabel' \/ 'PayCostQuestion' \/ 'QuestionWithSource' wrappers. We
-- mirror that exactly:
--
--   * wrappers are stripped by 'unwrapQuestion' before indexing;
--   * 'ChooseOneFromEach' indexes into @concat groups@;
--   * 'ChooseOneAtATimeWithAuto' reserves index @0@ for the auto-resolve-all
--     choice (real choices are @+1@) — we always answer @0@ (auto);
--   * every other index question indexes straight into its @choices@ list.
--
-- The returned 'QuestionResponse' carries @qrPlayerId = Just pid@ and
-- @qrQuestionVersion = Just gameScenarioSteps@ so the engine accepts it as
-- current rather than stale.
--
-- == Policy (v1 — coarse, deterministic, meant to be iterated)
--
-- We read the seat's situation (engaged enemies, modified stats, location
-- clues\/shroud, in-play enemy stats, priorities) once from a pure snapshot,
-- compute an /active focus/, then score each selectable choice. The active
-- focus is: 'aiFocusOverride' if set; else the current act's objective focus
-- when not engaged; else the investigator profile's dominant focus (tag
-- weights and deck focus, seeded by the highest skill).
--
-- Scoring is additive and intentionally simple — it is not chaos-bag math.
-- Fights prefer enemies we can finish (low remaining health, favourable to-hit
-- via @combat >= enemyFight@); evades fire when @agility >= enemyEvade@;
-- investigates fire only where there are clues and @intellect >= shroud@;
-- priority targets dominate; reaction\/fast abilities clear the "skip" baseline
-- so free value (e.g. an after-kill clue) is taken even off-focus; otherwise
-- the seat ends its turn rather than flailing.
module Arkham.Ai.Decision (
  decideAi,
  chooseIndexFor,
  scoreChoice,
  AiSituation (..),
  EnemyInfo (..),
  emptySituation,
) where

import Arkham.Prelude

import Arkham.Ability (abilityActions, abilityIsFastAbility, abilityIsReactionAbility)
import Arkham.Ability.Types (Ability, abilityCardCode, abilityIndex, abilityTarget)
import Arkham.Act.Types (Field (ActCard))
import Arkham.Ai.Focus (Focus (..), allFoci, focusForAction, focusForSkill)
import Arkham.Ai.State (AiPlayerState (..))
import Arkham.Ai.Tags (
  abFocuses,
  aoFocus,
  ctFocuses,
  itDeckFocus,
  itWeights,
  lookupAbilityTag,
  lookupActObjective,
  lookupCardTag,
  lookupInvestigatorTag,
 )
import Arkham.Card () -- brings the @HasCardCode Card@ instance into scope
import Arkham.Card.CardCode (toCardCode)
import Arkham.Classes.HasGame (HasGame, getGame)
import Arkham.Classes.Query (select, selectOne)
import Arkham.Enemy.Types (Field (EnemyEvade, EnemyFight, EnemyHealth, EnemyHealthDamage))
import Arkham.Game () -- brings the orphan @Tracing Identity@ instance into scope
import Arkham.Game.Base (gameScenarioSteps)
import Arkham.Helpers.Investigator (getMaybeLocation, modifiedStatsOf)
import Arkham.Id (EnemyId, InvestigatorId, PlayerId, ScenarioId (..))
import Arkham.Investigator.Types (Field (InvestigatorRemainingActions, InvestigatorResources))
import Arkham.Location.Types (Field (LocationClues, LocationShroud))
import Arkham.Matcher (enemyEngagedWith, pattern AnyAct, pattern AnyInPlayEnemy, pattern InvestigatorIsPlayer, pattern TheScenario)
import Arkham.Message (Message)
import Arkham.Projection (field)
import Arkham.Question
import Arkham.SkillType (SkillType (..))
import Arkham.Stats (Stats, statsSkillValue)
import Arkham.Target (Target (..))
import Arkham.Tracing (Tracing)
import Data.Map.Strict qualified as Map
import Entity.Answer (
  Answer (..),
  AmountsResponse (..),
  PaymentAmountsResponse (..),
  QuestionResponse (..),
 )

-- | The entry point other agents call. Always returns a legal 'Answer'.
decideAi :: HasGame m => AiPlayerState -> PlayerId -> Question Message -> m Answer
decideAi state pid q = do
  g <- getGame
  -- The AI only inspects state, so we evaluate every read-only query against a
  -- pure snapshot in @ReaderT Game Identity@ (which is both 'HasGame' and
  -- 'Tracing'). That keeps 'decideAi''s constraint to just 'HasGame'.
  let sit = runIdentity (runReaderT (gatherSituation state pid) g)
  pure (buildAnswer sit pid (gameScenarioSteps g) q)

-- * Situation

-- | Coarse, snapshot stats for a single in-play enemy. Remaining health is
-- @Nothing@ for enemies that cannot be defeated by damage.
data EnemyInfo = EnemyInfo
  { eiRemainingHealth :: Maybe Int
  , eiFight :: Maybe Int
  , eiEvade :: Maybe Int
  }
  deriving stock (Show)

-- | Everything the policy needs, read once per decision.
data AiSituation = AiSituation
  { aiState :: AiPlayerState
  , aiIid :: Maybe InvestigatorId
  , aiActiveFocus :: Focus
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
  }
  deriving stock (Show)

-- | A situation with no seat resolved (used only when the player has no
-- investigator, e.g. mid-setup). Focus still comes from the configured profile.
emptySituation :: AiPlayerState -> AiSituation
emptySituation state =
  AiSituation
    { aiState = state
    , aiIid = Nothing
    , aiActiveFocus = profileFocus state Nothing
    , aiEngaged = []
    , aiStats = Nothing
    , aiResources = 0
    , aiRemainingActions = 0
    , aiLocationClues = 0
    , aiLocationShroud = Nothing
    , aiEnemies = mempty
    , aiPrioritySet = aiPriorities state
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
      focus <- computeActiveFocus state (notNull engaged) (Just stats)
      pure
        AiSituation
          { aiState = state
          , aiIid = Just iid
          , aiActiveFocus = focus
          , aiEngaged = engaged
          , aiStats = Just stats
          , aiResources = resources
          , aiRemainingActions = remActions
          , aiLocationClues = locClues
          , aiLocationShroud = locShroud
          , aiEnemies = Map.fromList enemyInfos
          , aiPrioritySet = aiPriorities state
          }

-- * Active focus

computeActiveFocus :: (HasGame n, Tracing n) => AiPlayerState -> Bool -> Maybe Stats -> n Focus
computeActiveFocus state engaged mStats = case aiFocusOverride state of
  Just f -> pure f
  Nothing -> do
    mActFocus <- if engaged then pure Nothing else actObjectiveFocus
    pure (fromMaybe (profileFocus state mStats) mActFocus)

actObjectiveFocus :: (HasGame n, Tracing n) => n (Maybe Focus)
actObjectiveFocus = do
  mScenario <- selectOne TheScenario
  mAct <- selectOne AnyAct
  case (mScenario, mAct) of
    (Just (ScenarioId scc), Just aid) -> do
      card <- field ActCard aid
      pure (aoFocus <$> lookupActObjective scc (toCardCode card))
    _ -> pure Nothing

-- | The investigator profile's dominant focus: tag weights (seeded by the
-- highest skill), falling back to the deck focus, then to the strongest stat.
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

-- | The engine index to answer for an index-style question, or 'Nothing' for a
-- shape that needs a non-index 'Answer' (or that we do not model). Exposed for
-- unit testing against synthetic 'Question's.
chooseIndexFor :: AiSituation -> Question Message -> Maybe Int
chooseIndexFor sit = go . unwrapQuestion
 where
  go = \case
    -- index 0 is the auto "resolve all in any order" choice and is always legal
    ChooseOneAtATimeWithAuto _ _ -> Just 0
    q -> case flattenChoices q of
      Just (cs, off) -> Just (off + pickFrom q cs)
      Nothing -> Nothing
  pickFrom q cs
    -- Batch (multi-select) windows: stay conservative. Take the explicit
    -- stop/skip when offered (don't waste cards/resources); otherwise the
    -- window is mandatory, so pick the first selectable choice.
    | isBatch q = fromMaybe (firstSelectable cs) (firstStop cs)
    | otherwise = bestByScore sit cs

-- | Strip the wrappers the engine peels before indexing.
unwrapQuestion :: Question msg -> Question msg
unwrapQuestion = \case
  QuestionLabel _ _ q -> unwrapQuestion q
  PayCostQuestion _ q -> unwrapQuestion q
  QuestionWithSource _ _ q -> unwrapQuestion q
  q -> q

-- | The flattened, engine-indexed choice list and the index offset for a
-- question (offset is non-zero only for shapes that reserve leading indices).
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

-- | Score a single choice for the current situation; higher is more attractive.
-- Pure and deterministic so it can be unit-tested with synthetic choices. The
-- weights are coarse v1 heuristics to be tuned with play data, not a solved
-- policy.
scoreChoice :: AiSituation -> UI Message -> Int
scoreChoice sit ui = priorityScore + kindScore + stopScore
 where
  active = aiActiveFocus sit
  skill s = (\st -> statsSkillValue st s) <$> aiStats sit
  alignBonus f = if f == active then 4 else 0

  priorityScore = case uiTarget ui of
    Just t | t `elem` aiPrioritySet sit -> 50
    _ -> 0

  -- "Stop" baselines: skipping a reaction is neutral-ish (5); ending the turn
  -- is the lowest-value stop (1) so any worthwhile action outscores it.
  stopScore = case ui of
    EndTurnButton {} -> 1
    Done {} -> 5
    SkipTriggersButton {} -> 5
    _ -> 0

  kindScore = case classifyUI ui of
    FightChoice eid -> 4 + alignBonus CombatFocus + combatBonus eid
    EvadeChoice eid -> 2 + alignBonus EvadeFocus + evadeBonus eid
    InvestigateChoice -> case investigateBonus of
      0 -> 0
      n -> n + alignBonus InvestigateFocus
    FocusedChoice f -> 6 + alignBonus f
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
  = FightChoice EnemyId
  | EvadeChoice EnemyId
  | InvestigateChoice
  | FocusedChoice Focus
  | PlainChoice

classifyUI :: UI Message -> ChoiceKind
classifyUI = \case
  FightLabel eid _ -> FightChoice eid
  FightLabelWithSkill eid _ _ -> FightChoice eid
  EvadeLabel eid _ -> EvadeChoice eid
  EvadeLabelWithSkill eid _ _ -> EvadeChoice eid
  EngageLabel eid _ -> FightChoice eid
  ui@(AbilityLabel _ ab _ _ _)
    -- Reaction/fast abilities are free triggers; treat as focused value so
    -- they clear the skip baseline even when off the active focus.
    | abilityIsReactionAbility ab || abilityIsFastAbility ab ->
        maybe PlainChoice FocusedChoice (abilityFocus ab)
    | otherwise -> fromFocusAndEnemy (abilityFocus ab) (uiEnemyId ui)
  ui -> fromFocusAndEnemy (uiFocus ui) (uiEnemyId ui)

fromFocusAndEnemy :: Maybe Focus -> Maybe EnemyId -> ChoiceKind
fromFocusAndEnemy mf me = case (mf, me) of
  (Just CombatFocus, Just eid) -> FightChoice eid
  (Just EvadeFocus, Just eid) -> EvadeChoice eid
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

componentFocus :: Component -> Maybe Focus
componentFocus = \case
  InvestigatorDeckComponent _ -> Just SupportFocus
  InvestigatorComponent _ ResourceToken -> Just SupportFocus
  _ -> Nothing

abilityFocus :: Ability -> Maybe Focus
abilityFocus ab =
  (lookupAbilityTag (abilityCardCode ab) (abilityIndex ab) >>= headMay . abFocuses)
    <|> listToMaybe (mapMaybe focusForAction (abilityActions ab))
    <|> (lookupCardTag (abilityCardCode ab) >>= headMay . ctFocuses)

-- * Amount fallbacks (deep fallback shapes)

-- | Build a minimal, legal amount distribution. For total\/min\/one-of targets
-- we greedily fill choices up to the target within their bounds; for a max
-- target (or none) we give each choice its lower bound.
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
