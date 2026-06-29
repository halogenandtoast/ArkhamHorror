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

== Situational awareness (v4 additions)

Four cheap, snapshot-read signals sharpen the policy:

  * /Clue-spend objectives/. For an act that wants the GROUP to gather then
    spend clues (e.g. The Barrier's @SpendClues PerPlayer 3 \@ Hallway@) we read
    the group's spendable clues ('aiGroupClues'), the resolved target
    ('aiClueTarget'), and the scoped spend location ('aiSpendLocation'). Movement
    is two-phase — gather toward the nearest clue location while short, then walk
    back to the spend location once the group has enough — and scoring drops the
    investigate bonus to 0 once the target is met, so the seat stops
    over-investigating and heads to the Hallway.

  * /Weapon management/. Each controlled asset gets a keep value
    ('aiAssetQuality'): a weapon's per-attack 'abDamage', else a fixed mid value.
    A forced slot-discard drops the LOWEST (the Knife, never the .38). Playing a
    redundant or strictly-worse weapon while we already field one is pushed below
    ending the turn; a strictly better weapon is still a fine upgrade.

  * /Doom pressure/ ('aiDoomPressure' = doom / agenda threshold). At\/above
    'doomPressureThreshold' the seat bumps the act objective (fight its target,
    gather its clues) and suppresses economy, closing out before the agenda
    advances.

  * /Self-preservation/. At low remaining health ('aiRemainingHealth') the seat
    eases off attacking a dangerous enemy it cannot finish and favours evading
    (also when sanity is low). And on the last action ('aiRemainingActions')
    economy never displaces a genuinely productive action.

== Teammate awareness (v5 additions)

'gatherSituation' also snapshots every /other/ investigator at the table into
'aiTeammates' (a list of 'TeammateInfo'), measured with the very helpers the
seat uses for itself — location, modified combat\/intellect, best per-attack
weapon damage, AI deck focus, and engaged enemies. The list is empty in a solo
game (or when the AI seat is the only investigator), and three bounded
coordination nudges in 'scoreChoice' are /all/ gated on it being non-empty, so
the solo path is unchanged byte-for-byte. None of them is a veto: they only
re-rank already-legal choices, never resurrect an illegal one, and never beat a
directive ('aiPrioritySet').

  * /No piling on/. A Fight on an enemy a /co-engaged/ teammate can already
    finish soon (their combat clears its fight value and twice their weapon
    damage covers its remaining health, i.e. ~2 turns) is nudged down by
    'teammateHandledPenalty' so the seat does something else — unless that enemy
    is a priority target, or no co-engaged teammate can actually finish it (in
    which case helping is good and there is no penalty).

  * /Role-aware investigate deferral/. An Investigate where a co-located,
    clearly-better investigator stands (higher intellect, or an investigate deck
    focus the seat lacks) can clear the location's clues themselves
    ('deferClueCoverage') is nudged down by 'investigateDeferralPenalty', letting
    the better investigator take them while the seat fights\/supports. The strongest investigator present is
    unaffected.

  * /Specialty division of labor/. Derived from the seat's own specialty
    ('seatSpecialty'): a combat-specialty seat earns a small (±'specialtyDivisionDelta')
    bonus to fighting and malus to investigating when the party fields a stronger
    investigator; a clue-specialty seat earns the reverse when the party fields a
    stronger fighter. Kept ≤ ±4 so kill-range, clue presence, priorities, and
    doom pressure still dominate.
-}
module Arkham.Ai.Decision (
  decideAi,
  isAssistCommitWindow,
  decideAiAssist,
  chooseIndexFor,
  scoreChoice,
  scoreBreakdown,
  choiceFeatures,
  learnedScore,
  -- Exposed for the offline ML extraction tool (app-extract): the read-once
  -- situation snapshot, the engine-indexed choice flattener, and the wrapper
  -- stripper. These are the exact entry points the live decision path uses, so
  -- the extracted dataset is distilled against the same code.
  gatherSituation,
  flattenChoices,
  unwrapQuestion,
  damageAssignmentShape,
  damageAssignmentDecision,
  slotDiscardShape,
  slotDiscardDecision,
  AiSituation (..),
  EnemyInfo (..),
  SkillTestInfo (..),
  TeammateInfo (..),
  TokenOutcome (..),
  passOdds,
  emptySituation,
) where

import Arkham.Prelude

import Arkham.Ability (abilityActions, abilityIsFastAbility, abilityIsReactionAbility)
import Arkham.Ability.Types (Ability, abilityCardCode, abilityIndex, abilityTarget)
import Arkham.Act.Types (Field (ActCard))
import Arkham.Agenda.Types (Field (AgendaCard))
import Arkham.Ai.Focus (Focus (..), allFoci, focusForAction, focusForSkill)
import Arkham.Ai.Model (LinearModel (..), activeLinearModel)
import Arkham.Ai.State (AiPlayerState (..))
import Arkham.Ai.Tags (
  ActObjective,
  GameValueSpec (..),
  InvestigatorTag,
  Objective (..),
  SoakTarget (..),
  SoakTrigger (..),
  abDamage,
  abFocuses,
  agThreshold,
  aoFocus,
  aoObjective,
  ctAbilities,
  ctFocuses,
  ctRole,
  ctSoakTrigger,
  ctWeight,
  itDeckFocus,
  itWeights,
  lookupAbilityTag,
  lookupActObjective,
  lookupCardTag,
  lookupInvestigatorTag,
  lookupScenarioTag,
  stAgendas,
 )
import Arkham.Asset.Types (Field (AssetCard, AssetRemainingHealth))
import Arkham.Card (Card, printedCardCost)
import Arkham.Card.CardCode (CardCode, toCardCode)
import Arkham.Card.CardDef (cdCardSubType, cdCardTraits, cdCardType, cdSkills, toCardDef)
import Arkham.Card.CardType (CardType (AssetType, EventType))
import Arkham.ChaosToken (
  ChaosTokenModifier (AutoFailModifier, AutoSuccessModifier),
  ChaosTokenValue (..),
  chaosTokenModifierToInt,
 )
import Arkham.Classes.HasChaosTokenValue (getChaosTokenValue)
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
  Field (EnemyEvade, EnemyFight, EnemyHealthDamage, EnemyLocation, EnemyRemainingHealth),
 )
import Arkham.Game ()
-- brings the orphan @Tracing Identity@ instance into scope
import Arkham.Game.Base (gameScenarioSteps)
import Arkham.GameEnv (getDistance, getSkillTest)
import Arkham.Helpers (unDeck)
import Arkham.Helpers.ChaosBag (getOnlyChaosTokensInBag)
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.Investigator (getHandSize, getMaybeLocation, modifiedStatsOf)
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Helpers.Query (getInvestigators, getPlayerCount)
import Arkham.Helpers.SkillTest (
  getSkillTestDifficulty,
  getSkillTestInvestigator,
  getSkillTestMatchingSkillIcons,
  getSkillTestModifiedSkillValue,
 )
import Arkham.Id (AssetId, EnemyId, InvestigatorId, LocationId, PlayerId, ScenarioId (..), unInvestigatorId)
import Arkham.Investigator.Types (
  Field (
    InvestigatorDeck,
    InvestigatorHand,
    InvestigatorRemainingActions,
    InvestigatorRemainingHealth,
    InvestigatorRemainingSanity,
    InvestigatorResources
  ),
 )
import Arkham.Location.Types (Field (LocationClues, LocationShroud))
import Arkham.Matcher (
  assetControlledBy,
  enemyEngagedWith,
  enemyIs,
  notInvestigator,
  pattern AnyAct,
  pattern AnyAgenda,
  pattern AnyEnemy,
  pattern AnyInPlayEnemy,
  pattern InvestigatorIsPlayer,
  pattern LocationWithAnyClues,
  pattern TheScenario,
 )
import Arkham.Matcher.Location (
  LocationMatcher (Anywhere, ClosestPathLocation, LocationWithEnemy, LocationWithId, LocationWithTitle, UnrevealedLocation),
 )
import Arkham.Message (
  Message,
  pattern Discard,
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
import Arkham.Trait (Trait (Weapon))
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
  g <- getGame
  -- Resolve the performer's current pass odds in the pure snapshot (like
  -- 'decideAi'), keeping the public constraint at 'HasGame'. Don't burn an assist
  -- card onto a test the performer has already locked (odds >= the commit
  -- threshold); an unknown bag (Nothing) falls through to the old always-commit.
  let steps = gameScenarioSteps g
      mOdds = runIdentity (runReaderT assistPerformerOdds g)
  pure $
    if maybe False (>= commitOddsThreshold) mOdds
      then Nothing
      else case flattenChoices (unwrapQuestion q0) of
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
  -- ^ health still on the enemy (total health minus damage tokens), @Nothing@
  -- when it cannot be defeated by damage.
  , eiAttackDamage :: Int
  -- ^ the health damage this enemy deals per attack ('EnemyHealthDamage'); the
  -- self-preservation gate treats an engaged enemy as dangerous when this meets
  -- or exceeds our remaining health.
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

{- | The resolved effect of a single chaos token on a skill test, captured once
per decision (in 'resolveBagOutcomes') so 'passOdds' can compute a pass
probability without re-querying the bag per choice. 'OutcomeMod' carries the
token's numeric modifier; the two auto outcomes short-circuit the comparison.
-}
data TokenOutcome = OutcomeFail | OutcomeSucceed | OutcomeMod Int
  deriving stock (Eq, Show)

{- | P(success) of a single uniform chaos-token draw against a skill value and a
difficulty: the fraction of outcomes that pass. An 'OutcomeSucceed' always
passes, an 'OutcomeFail' never does, and an 'OutcomeMod m' passes iff
@skillValue + m >= difficulty@. 'Nothing' when the outcome list is empty (the
bag is unknown — no scenario / between scenarios — so callers fall back to the
prior coarse heuristics). Uses 'foldl'', never the partial ClassyPrelude
'maximum'.
-}
passOdds :: [TokenOutcome] -> Int -> Int -> Maybe Double
passOdds outcomes skillValue difficulty = case outcomes of
  [] -> Nothing
  _ ->
    let denom = length outcomes
        numer = foldl' (\acc o -> if passes o then acc + 1 else acc) (0 :: Int) outcomes
     in Just (fromIntegral numer / fromIntegral denom)
 where
  passes = \case
    OutcomeSucceed -> True
    OutcomeFail -> False
    OutcomeMod m -> skillValue + m >= difficulty

{- | A coarse, snapshot read of one /other/ investigator at the table, captured
once per decision so the seat can coordinate rather than act in a vacuum. Every
field is read with the same helpers the seat reads for itself (and that
'Arkham.Ai.Questions' uses), so the two stay consistent.
-}
data TeammateInfo = TeammateInfo
  { tmLocation :: Maybe LocationId
  -- ^ the teammate's current location ('getMaybeLocation'); 'Nothing' when off the map.
  , tmIntellect :: Int
  -- ^ their modified intellect ('modifiedStatsOf'); the investigate-deferral yardstick.
  , tmCombat :: Int
  -- ^ their modified combat ('modifiedStatsOf'); the fight to-hit yardstick.
  , tmWeaponDamage :: Int
  {- ^ the most damage a single controlled weapon's tagged Fight ability deals per
  attack ('abDamage', the same 'weaponDamageOf' fold the seat uses), floored at 1.
  -}
  , tmDeckFocus :: Maybe Focus
  {- ^ their AI profile's deck focus ('itDeckFocus'), when the controlled
  investigator is tagged in @ai-tags.json@; 'Nothing' for an untagged seat.
  -}
  , tmEngaged :: [EnemyId]
  -- ^ the enemies engaged with them ('enemyEngagedWith'); who is already busy with what.
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
  , aiLocation :: Maybe LocationId
  {- ^ the seat's own current location; the co-location yardstick for the
  role-aware investigate-deferral teammate nudge ('scoreChoice').
  -}
  , aiLocationClues :: Int
  , aiLocationShroud :: Maybe Int
  , aiEnemies :: Map EnemyId EnemyInfo
  , aiTeammates :: [TeammateInfo]
  {- ^ the other investigators at the table (empty when solo / the only AI seat),
  read once in 'gatherSituation'. Drives the three bounded coordination nudges in
  'scoreChoice'; an empty list makes every one of them a no-op, so the solo path
  is unchanged.
  -}
  , aiPrioritySet :: [Target]
  , aiSkillTest :: Maybe SkillTestInfo
  -- ^ live skill-test snapshot; drives the commit-window special case.
  , aiBagOutcomes :: [TokenOutcome]
  {- ^ the live chaos bag resolved to one 'TokenOutcome' per physical token (so
  duplicate faces are count-weighted), read once in 'gatherSituation' via
  'resolveBagOutcomes'. Empty when there is no scenario / between scenarios, in
  which case every 'passOdds'-driven score falls back to its prior coarse value.
  Drives the odds-based fight\/evade\/investigate scoring and the commit decision.
  -}
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
  , aiWeaknessDrawRisk :: Double
  {- ^ fraction of the seat's deck that is a weakness (cards with a
  'cdCardSubType'), read once in 'gatherSituation'; 0 for an empty deck. Pushes
  the draw economy score down so the seat is wary of drawing when a weakness is
  likely on top.
  -}
  , aiSoakAssets :: Map AssetId (CardCode, Int)
  {- ^ each controlled asset's card code and current remaining health (soak room;
  'AssetRemainingHealth', @Nothing@ → 0). Drives the soak-trigger damage-routing
  in 'damageAssignmentDecision'.
  -}
  , aiBestWeaponDamage :: Int
  {- ^ the most damage a single controlled weapon's tagged Fight ability can deal
  per attack ('abDamage'), floored at 1 (the unarmed base attack). The kill-range
  yardstick for "would chipping this enemy bring it into one-attack reach?".
  -}
  , aiControlsWeapon :: Bool
  {- ^ we already control at least one Weapon-trait asset. Gates the card-play
  redundancy penalty so a second, no-better weapon (a Knife over the .38) is not
  played, while a strictly-better weapon upgrade still is.
  -}
  , aiAssetQuality :: Map AssetId Int
  {- ^ per-controlled-asset keep value: a weapon's tagged per-attack 'abDamage',
  or a fixed mid value ('nonWeaponQuality') for a non-weapon. The forced
  slot-discard prompt drops the LOWEST, so a Knife (1) is discarded before the
  .38\/Machete (2).
  -}
  , aiGroupClues :: Int
  {- ^ the whole group's spendable clues ('getSpendableClueCount' over
  'getInvestigators'); compared against 'aiClueTarget' to decide gather-vs-spend
  for a clue-spend act objective.
  -}
  , aiClueTarget :: Maybe Int
  {- ^ the resolved clue count a 'SpendCluesObjective' demands (per 'resolveSpec'),
  or 'Nothing' when the current act is not a clue-spend objective.
  -}
  , aiSpendLocation :: Maybe LocationId
  {- ^ the location a scoped clue-spend objective must be spent at (e.g. the
  Hallway), or 'Nothing' for an @"anywhere"@ scope (or no clue-spend objective).
  Movement walks back here once the group has enough clues.
  -}
  , aiObjectiveEnemy :: Maybe EnemyId
  {- ^ the in-play enemy the current act's 'DefeatEnemyObjective' targets, if any.
  Doom pressure bumps fighting precisely this enemy.
  -}
  , aiDoomPressure :: Double
  {- ^ current doom divided by the current agenda's advance threshold (0 when no
  agenda\/threshold is tagged). At\/above 'doomPressureThreshold' the seat leans
  into the act objective and stops spending actions on economy.
  -}
  , aiRemainingHealth :: Int
  -- ^ remaining health ('InvestigatorRemainingHealth'); gates self-preservation.
  , aiRemainingSanity :: Int
  -- ^ remaining sanity ('InvestigatorRemainingSanity'); also raises evade when low.
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
    , aiLocation = Nothing
    , aiLocationClues = 0
    , aiLocationShroud = Nothing
    , aiEnemies = mempty
    , aiTeammates = []
    , aiPrioritySet = aiPriorities state
    , aiSkillTest = Nothing
    , aiBagOutcomes = []
    , aiMoveTarget = Nothing
    , aiLocationDistances = mempty
    , aiResourceShortfall = 0
    , aiShouldDig = False
    , aiHandNotFull = True
    , aiWeaknessDrawRisk = 0
    , aiSoakAssets = mempty
    , aiBestWeaponDamage = 1
    , aiControlsWeapon = False
    , aiAssetQuality = mempty
    , aiGroupClues = 0
    , aiClueTarget = Nothing
    , aiSpendLocation = Nothing
    , aiObjectiveEnemy = Nothing
    , aiDoomPressure = 0
    , -- no seat resolved: keep the safety gate off with a high remaining-vitality default
      aiRemainingHealth = 99
    , aiRemainingSanity = 99
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
        -- Remaining health is total health minus damage tokens on the enemy
        -- ('EnemyRemainingHealth'); 'EnemyHealthDamage' is instead the enemy's
        -- per-attack damage, captured separately for the self-preservation gate.
        rh <- field EnemyRemainingHealth eid
        atk <- field EnemyHealthDamage eid
        f <- field EnemyFight eid
        ev <- field EnemyEvade eid
        pure (eid, EnemyInfo rh atk f ev)
      -- Teammate awareness: snapshot every OTHER investigator with the same reads
      -- the seat takes for itself (and 'Arkham.Ai.Questions' uses), so the policy
      -- can coordinate. Empty in solo / single-AI games, which is exactly what
      -- keeps the coordination nudges in 'scoreChoice' no-ops.
      teammates <- gatherTeammates iid
      -- Read hand + controlled in-play asset cards once; both the focus blend and
      -- the economy snapshot consume them, so we never re-query these fields.
      handCards <- field InvestigatorHand iid
      assetIds <- select (assetControlledBy iid)
      assetCards <- traverse (field AssetCard) assetIds
      -- Pair each controlled asset with its card code and current soak room; the
      -- damage-assignment special case routes attack damage onto soak-trigger
      -- assets (Guard Dog) using this.
      soakAssets <- for (zip assetIds assetCards) $ \(aid, card) -> do
        remaining <- field AssetRemainingHealth aid
        pure (aid, (toCardCode card, fromMaybe 0 remaining))
      mActObjective <- currentActObjective
      -- Clue-spend objective awareness: when the act wants the GROUP to gather
      -- then spend clues, snapshot the group's spendable clues, the resolved
      -- target, and the (scoped) spend location. Movement and scoring use these
      -- to route gather-then-spend rather than camping a clue location forever.
      (groupClues, clueTarget, spendLocation) <- case aoObjective <$> mActObjective of
        Just (SpendCluesObjective amount scope) -> do
          clues <- getSpendableClueCount =<< getInvestigators
          target <- resolveSpec amount
          spendLoc <-
            if scope == "anywhere" then pure Nothing else selectOne (LocationWithTitle scope)
          pure (clues, Just target, spendLoc)
        _ -> pure (0, Nothing, Nothing)
      -- The in-play enemy the current act's defeat-enemy objective targets, used
      -- to bump fighting precisely that enemy under doom pressure.
      objectiveEnemy <- case aoObjective <$> mActObjective of
        Just (DefeatEnemyObjective cc) -> selectOne (enemyIs cc)
        _ -> pure Nothing
      doomPressure <- gatherDoomPressure
      remHealth <- field InvestigatorRemainingHealth iid
      remSanity <- field InvestigatorRemainingSanity iid
      -- Resolve the chaos bag ONCE (one outcome per physical token); empty when
      -- there is no scenario, which keeps every odds-based score on its prior
      -- coarse fallback. Kept out of the per-choice hot path.
      bagOutcomes <- resolveBagOutcomes iid
      -- Weakness draw-risk: how much of the remaining deck is a weakness, used to
      -- discourage drawing when a weakness is likely to come up.
      deckCards <- unDeck <$> field InvestigatorDeck iid
      let
        mTag = lookupInvestigatorTag (aiInvestigatorCode state)
        deckSize = length deckCards
        weaknessCount = length (filter (isJust . cdCardSubType . toCardDef) deckCards)
        weaknessDrawRisk =
          if deckSize == 0 then 0 else fromIntegral weaknessCount / fromIntegral deckSize :: Double
        focusWeights =
          focusWeightsFor (aiFocusOverride state) mTag (Just stats) handCards assetCards mActObjective
        focus = computeActiveFocus state focusWeights
        -- The best single-attack damage from any controlled weapon's tagged Fight
        -- ability, floored at the unarmed base attack (1). Used as the kill-range
        -- yardstick when deciding whether a soak chip sets up a finisher.
        bestWeaponDamage = foldl' max 1 (mapMaybe weaponDamageOf assetCards)
        -- We already field a Weapon-trait asset (gates the redundancy penalty).
        controlsWeapon = any (member Weapon . cdCardTraits . toCardDef) assetCards
        -- Per-asset keep value: a weapon's per-attack damage, else a fixed mid
        -- value. The slot-discard prompt drops the LOWEST, so a Knife (1) goes
        -- before the .38 (2).
        assetQuality =
          Map.fromList [(aid, fromMaybe nonWeaponQuality (weaponDamageOf card)) | (aid, card) <- zip assetIds assetCards]
        -- A fighter (combat is its stronger stat) yields clue-gathering when the
        -- party holds a stronger investigator who can cover the clues — it should
        -- not chase clue locations (it just bounces and wastes its combat edge).
        -- 'canFight' then decides whether it positions toward an enemy (a real
        -- weapon) or simply holds with the team and sets up.
        yieldClues =
          statsSkillValue stats SkillCombat > statsSkillValue stats SkillIntellect
            && any ((> statsSkillValue stats SkillIntellect) . tmIntellect) teammates
        canFight = bestWeaponDamage >= 2
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
          groupClues
          clueTarget
          spendLocation
          (aiPriorities state)
          yieldClues
          canFight
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
          , aiLocation = mLoc
          , aiLocationClues = locClues
          , aiLocationShroud = locShroud
          , aiEnemies = Map.fromList enemyInfos
          , aiTeammates = teammates
          , aiPrioritySet = aiPriorities state
          , aiSkillTest = skillTestInfo
          , aiBagOutcomes = bagOutcomes
          , aiMoveTarget = moveTarget
          , aiLocationDistances = distances
          , aiResourceShortfall = ecResourceShortfall econ
          , aiShouldDig = ecShouldDig econ
          , aiHandNotFull = ecHandNotFull econ
          , aiWeaknessDrawRisk = weaknessDrawRisk
          , aiSoakAssets = Map.fromList soakAssets
          , aiBestWeaponDamage = bestWeaponDamage
          , aiControlsWeapon = controlsWeapon
          , aiAssetQuality = assetQuality
          , aiGroupClues = groupClues
          , aiClueTarget = clueTarget
          , aiSpendLocation = spendLocation
          , aiObjectiveEnemy = objectiveEnemy
          , aiDoomPressure = doomPressure
          , aiRemainingHealth = remHealth
          , aiRemainingSanity = remSanity
          }

{- | Snapshot every /other/ investigator at the table into a 'TeammateInfo'. The
list is empty in a solo game (or when the AI seat is the only investigator),
which is the invariant the coordination nudges in 'scoreChoice' rely on to stay
no-ops on the solo path. Each field uses the same helper the seat reads for
itself, so a teammate is measured exactly as the seat measures itself:

  * location via 'getMaybeLocation';
  * combat\/intellect via 'modifiedStatsOf';
  * best per-attack weapon damage via the same 'weaponDamageOf' fold (floored at
    the unarmed base attack, 1);
  * deck focus via the controlled investigator's @ai-tags.json@ profile
    ('lookupInvestigatorTag' on its 'unInvestigatorId' card code → 'itDeckFocus'),
    'Nothing' when untagged;
  * engaged enemies via 'enemyEngagedWith'.
-}
gatherTeammates :: (HasGame n, Tracing n) => InvestigatorId -> n [TeammateInfo]
gatherTeammates iid = do
  tmIids <- select (notInvestigator iid)
  for tmIids $ \tmIid -> do
    tmLoc <- getMaybeLocation tmIid
    tmStats <- modifiedStatsOf Nothing tmIid
    engagedEnemies <- select (enemyEngagedWith tmIid)
    tmAssetCards <- traverse (field AssetCard) =<< select (assetControlledBy tmIid)
    pure
      TeammateInfo
        { tmLocation = tmLoc
        , tmIntellect = statsSkillValue tmStats SkillIntellect
        , tmCombat = statsSkillValue tmStats SkillCombat
        , -- same fold as the seat's 'aiBestWeaponDamage' (floored at the unarmed 1)
          tmWeaponDamage = foldl' max 1 (mapMaybe weaponDamageOf tmAssetCards)
        , tmDeckFocus = itDeckFocus <$> lookupInvestigatorTag (unInvestigatorId tmIid)
        , tmEngaged = engagedEnemies
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

{- | Resolve a tagged 'GameValueSpec' to a concrete count in the live snapshot: a
static value verbatim, a per-player value scaled by 'getPlayerCount'.
-}
resolveSpec :: (HasGame m, Tracing m) => GameValueSpec -> m Int
resolveSpec = \case
  StaticV n -> pure n
  PerPlayerV n -> (n *) <$> getPlayerCount

{- | Doom pressure: current doom over the current agenda's advance threshold, in
@[0, ∞)@ (typically @0..1@). 0 when there is no scenario\/agenda, the agenda has
no ai-tag, or its threshold resolves non-positive. Read once and stored as
'aiDoomPressure'; gating happens in 'scoreChoice'.
-}
gatherDoomPressure :: (HasGame n, Tracing n) => n Double
gatherDoomPressure = do
  mScenario <- selectOne TheScenario
  mAgenda <- selectOne AnyAgenda
  case (mScenario, mAgenda) of
    (Just (ScenarioId scc), Just aid) -> do
      card <- field AgendaCard aid
      case lookupScenarioTag scc >>= Map.lookup (toCardCode card) . stAgendas of
        Nothing -> pure 0
        Just info -> do
          threshold <- resolveSpec (agThreshold info)
          doom <- getDoomCount
          pure $ if threshold <= 0 then 0 else fromIntegral doom / fromIntegral threshold
    _ -> pure 0

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

-- * Chaos bag odds

{- | Resolve the live chaos bag into one 'TokenOutcome' per /physical/ token, so
duplicate faces are count-weighted in 'passOdds'. Returns '[]' when the bag is
empty (no scenario / between scenarios): the '()' 'getChaosTokenValue' instance
errors with no scenario, so we resolve only when the bag is non-empty. Each
DISTINCT face is resolved exactly once into a @Map ChaosTokenFace TokenOutcome@,
then expanded back across every physical token — keeping the per-face value query
out of any per-choice hot path. 'AutoFailModifier' \/ 'AutoSuccessModifier' map to
the short-circuit outcomes; every other modifier goes through
'chaosTokenModifierToInt' (whose 'Nothing' only arises for the auto modifiers we
already handled, so we treat it defensively as @OutcomeMod 0@).
-}
resolveBagOutcomes :: (HasGame n, Tracing n) => InvestigatorId -> n [TokenOutcome]
resolveBagOutcomes iid = do
  tokens <- getOnlyChaosTokensInBag
  if null tokens
    then pure []
    else do
      let faces = Set.toList (Set.fromList (map (.face) tokens))
      resolved <- for faces $ \face -> do
        ChaosTokenValue _ modifier <- getChaosTokenValue iid face ()
        outcome <- case modifier of
          AutoFailModifier -> pure OutcomeFail
          AutoSuccessModifier -> pure OutcomeSucceed
          _ -> OutcomeMod . fromMaybe 0 <$> chaosTokenModifierToInt modifier
        pure (face, outcome)
      let outcomeMap = Map.fromList resolved
      pure [Map.findWithDefault (OutcomeMod 0) t.face outcomeMap | t <- tokens]

{- | The performer's current pass odds in the open skill test, resolved against
the live bag using the performer's own token values, or 'Nothing' when there is
no open test, no resolvable performer, no difficulty, or an empty bag. Used by
'decideAiAssist' so the seat does not burn an assist card onto a test the
performer has already locked.
-}
assistPerformerOdds :: (HasGame n, Tracing n) => n (Maybe Double)
assistPerformerOdds =
  getSkillTest >>= \case
    Nothing -> pure Nothing
    Just _ ->
      getSkillTestInvestigator >>= \case
        Nothing -> pure Nothing
        Just performer -> do
          outcomes <- resolveBagOutcomes performer
          cur <- getSkillTestModifiedSkillValue
          mDiff <- getSkillTestDifficulty
          pure (mDiff >>= passOdds outcomes cur)

-- * Movement targeting

{- | Compute the immediate one-hop 'aiMoveTarget' and the distance map used to
break ties in the move-destination prompt. We never move while engaged (we
fight in place). Otherwise a user-set /priority/ location\/enemy reroutes
movement ahead of everything else; absent a priority we route toward clues
(investigate focus) or the act's defeat-enemy target (combat focus). Every
read is guarded so a missing location\/enemy just yields 'Nothing'.

For a clue-spend act objective the investigate route is two-phase: while the
group is still short of the target (@groupClues < target@) we keep heading to
the nearest clue location to gather; once the group has enough
(@groupClues >= target@) we walk back to the scoped spend location (the Hallway)
so the clues can actually be cashed in — an @"anywhere"@ scope just stays put.
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
  -> Int
  -> Maybe Int
  -> Maybe LocationId
  -> [Target]
  -> Bool
  -- ^ yield clue-gathering to a stronger investigator (route as a combat seat)
  -> Bool
  -- ^ can meaningfully fight (a real weapon) — gates positioning toward an enemy
  -> n (Maybe LocationId, Map LocationId Int)
gatherMovement iid focus engaged mHere locClues mShroud stats mObjective groupClues mClueTarget mSpendLocation priorities yieldClues canFight
  | engaged = pure (Nothing, mempty)
  | otherwise = do
      mPriorityDest <- priorityDestination
      (mStep, mDest) <- case mPriorityDest of
        Just dest -> routeToward dest
        Nothing -> case effFocus of
          InvestigateFocus -> case (mObjective, mClueTarget) of
            -- Enough clues gathered: head to the spend location instead of
            -- camping a clue location. "anywhere" scope stays put.
            (Just (SpendCluesObjective {}), Just target)
              | groupClues >= target -> maybe (pure (Nothing, Nothing)) routeToward mSpendLocation
            _ -> gatherClues
          CombatFocus -> case mObjective of
            Just (DefeatEnemyObjective cc) -> enemyRoute cc
            -- No act-defined enemy: a fighter gravitates to the nearest enemy on
            -- the board to engage/tank it — but only with a real weapon
            -- ('canFight'). An unarmed seat holds with the team and sets up.
            _ -> if canFight then nearestEnemyRoute else pure (Nothing, Nothing)
          _ -> pure (Nothing, Nothing)
      distances <- maybe (pure mempty) distancesTo mDest
      pure (mStep, distances)
 where
  -- A fighter yielding clue-gathering to a stronger investigator routes as a
  -- combat seat (toward the nearest enemy / its objective target) instead of
  -- chasing clue locations it is poorly suited for.
  effFocus = if yieldClues then CombatFocus else focus

  -- One-hop step toward the nearest board enemy's location (for a combat seat
  -- with no act-defined target). Stays put when there is no enemy or no path.
  nearestEnemyRoute = case mHere of
    Nothing -> pure (Nothing, Nothing)
    Just here -> do
      enemyLocs <- select (LocationWithEnemy AnyEnemy)
      nearestLocation here enemyLocs >>= \case
        Nothing -> pure (Nothing, Nothing)
        Just dest -> routeToward dest

  worthInvestigatingHere =
    locClues > 0 && maybe True (statsSkillValue stats SkillIntellect >=) mShroud

  -- Gather clues: stay if this location is worth investigating, else route to
  -- the nearest clue location.
  gatherClues
    | worthInvestigatingHere = pure (Nothing, Nothing)
    | otherwise = do
        routed@(_, mDest) <- investigateRoute
        case mDest of
          Just _ -> pure routed -- an easier/other revealed clue location to head to
          -- No other reachable clue location. If we're standing on clues, STAY and
          -- take what we can (even at marginal odds) rather than wandering off to
          -- explore — leaving a clue location to look for clues elsewhere bounces
          -- us between it and the rooms beyond (the Hallway<->Cellar oscillation).
          -- Only explore unrevealed rooms when we are NOT already on clues.
          Nothing
            | locClues > 0 -> pure (Nothing, Nothing)
            | otherwise -> exploreRoute

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

  -- Explore: the objective still wants clues but no revealed clue location
  -- remains, so head to the nearest reachable UNREVEALED location — entering it
  -- reveals it and places fresh clues to gather. Prefer an adjacent one.
  exploreRoute = do
    oneHop <- getCanMoveToMatchingLocations iid GameSource UnrevealedLocation
    case oneHop of
      (lid : _) -> pure (Just lid, Just lid)
      [] -> case mHere of
        Nothing -> pure (Nothing, Nothing)
        Just here -> do
          unrevealed <- select UnrevealedLocation
          nearestLocation here unrevealed >>= \case
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
buildAnswer sit pid steps q0
  -- Enemy-attack damage/horror assignment (one point at a time): route the point
  -- onto a soak-trigger ally (Guard Dog) when that chips a killable enemy, else
  -- fall through to the generic pick. Detected on the raw question so we can read
  -- the attacker off the 'QuestionWithSource' wrapper before it is stripped.
  | Just (attacker, cs) <- damageAssignmentShape q0 =
      idx (damageAssignmentDecision sit attacker cs)
  -- Forced slot-discard (a ChooseOne whose every choice discards one of our
  -- assets): drop the lowest-quality asset (the Knife), never the .38. Handled
  -- before generic scoring, exactly like the damage-assignment shape.
  | Just cs <- slotDiscardShape q0 =
      idx (slotDiscardDecision sit cs)
  | otherwise = case unwrapQuestion q0 of
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

-- * Enemy-attack damage assignment (soak-trigger routing)

{- | Detect an enemy-attack damage\/horror assignment question (the engine parks
one point at a time): an optional 'QuestionWithSource' \/ 'QuestionLabel'
wrapper around a bare 'ChooseOne' whose every selectable choice assigns a
damage or horror token to an investigator or asset. Returns the attacking enemy
(read from the 'QuestionWithSource' source, when present) and the flattened
choice list (offset 0, so its indices are the engine's). 'Nothing' for any
other shape, so non-assignment questions route through the generic path
unchanged.
-}
damageAssignmentShape :: Question Message -> Maybe (Maybe EnemyId, [UI Message])
damageAssignmentShape = \case
  QuestionWithSource src _ inner -> (\cs -> (src.enemy, cs)) <$> damageChoices inner
  q -> (\cs -> (Nothing, cs)) <$> damageChoices q
 where
  damageChoices = \case
    QuestionLabel _ _ q -> damageChoices q
    ChooseOne cs ->
      let selectable = filter isSelectable cs
       in if not (null selectable) && all isDamageOrHorrorComponent selectable
            then Just cs
            else Nothing
    _ -> Nothing

-- | Whether a choice assigns a damage or horror token to an investigator or asset.
isDamageOrHorrorComponent :: UI Message -> Bool
isDamageOrHorrorComponent = \case
  ComponentLabel (InvestigatorComponent _ DamageToken) _ -> True
  ComponentLabel (InvestigatorComponent _ HorrorToken) _ -> True
  ComponentLabel (AssetComponent _ DamageToken) _ -> True
  ComponentLabel (AssetComponent _ HorrorToken) _ -> True
  _ -> False

{- | Scores for routing a point of attack damage onto a soak-trigger asset. A
/kill setup/ (the chip drops the enemy into one-attack kill range) beats a mere
/chip/ (free damage that also spares the controller), and both beat the generic
pick (which scores soak choices at 0).
-}
killSetupScore, chipScore :: Int
killSetupScore = 2
chipScore = 1

{- | Pick the choice index for an enemy-attack damage assignment. Prefer a soak
asset whose ai-tags carry a 'ctSoakTrigger' (e.g. Guard Dog, which damages the
attacker when it soaks) while it still has soak room and there is an enemy worth
chipping — strongly when the chip drops that enemy into one-attack kill range
('aiBestWeaponDamage'), moderately for a free chip that also spares the
controller (taken only while a point still leaves the ally alive, to avoid
spending its last soak on a mere chip). Falls back to the generic 'bestByScore'
pick (index 0 / current behavior) when no soak trigger helps, so non-trigger
assignments are unchanged. The returned index is into @cs@, the wrapper-stripped
'ChooseOne' list (offset 0).
-}
damageAssignmentDecision :: AiSituation -> Maybe EnemyId -> [UI Message] -> Int
damageAssignmentDecision sit attacker cs = case maxesBy snd beneficial of
  ((i, _) : _) -> i
  [] -> bestByScore sit cs
 where
  beneficial =
    [ (i, sc)
    | (i, AssetDamageLabel aid _) <- withIndex cs
    , Just (cc, remaining) <- [Map.lookup aid (aiSoakAssets sit)]
    , remaining > 0
    , Just tag <- [lookupCardTag cc]
    , Just trig <- [ctSoakTrigger tag]
    , let sc = soakValue remaining trig
    , sc > 0
    ]
  -- Best chip score this trigger can produce against any enemy it can hit.
  soakValue remaining trig = foldl' max 0 (map (chipValue remaining trig) (chipTargets trig))
  -- The enemies a trigger can damage: the attacker for Guard Dog; any enemy
  -- engaged with (hence colocated with) the seat for a colocated trigger.
  chipTargets trig = case stTarget trig of
    SoakAttacker -> [info | Just eid <- [attacker], Just info <- [Map.lookup eid (aiEnemies sit)]]
    SoakColocated -> mapMaybe (`Map.lookup` aiEnemies sit) (aiEngaged sit)
  -- Value of chipping one enemy: a kill setup when the chip brings it into
  -- one-attack reach (and it is not already dead); a plain chip otherwise, but
  -- only while the point would not spend the ally's last soak (remaining > 1).
  chipValue remaining trig info = case eiRemainingHealth info of
    Just rh
      | rh <= 0 -> 0
      | rh - stDamageToEnemy trig <= aiBestWeaponDamage sit -> killSetupScore
      | remaining > 1 -> chipScore
      | otherwise -> 0
    Nothing -> 0

-- * Forced slot-discard (drop the worst asset)

{- | Detect a forced asset slot-discard 'ChooseOne': every selectable choice is a
'TargetLabel' on an 'AssetTarget' whose first message is a 'Discard' (the
@RefillSlots@ over-capacity prompt, and the explicit choose-and-discard-asset
prompt). Returns the wrapper-stripped choice list (offset 0). 'Nothing' for any
other shape so it falls through to generic scoring.
-}
slotDiscardShape :: Question Message -> Maybe [UI Message]
slotDiscardShape q0 = case unwrapQuestion q0 of
  ChooseOne cs ->
    let selectable = filter isSelectable cs
     in if not (null selectable) && all isAssetDiscardChoice selectable then Just cs else Nothing
  _ -> Nothing

-- | Whether a choice discards one of the seat's assets (its first message is a 'Discard').
isAssetDiscardChoice :: UI Message -> Bool
isAssetDiscardChoice = \case
  TargetLabel (AssetTarget _) (Discard {} : _) -> True
  _ -> False

{- | Pick which asset to drop when forced to discard one: the controlled asset
with the LOWEST 'aiAssetQuality' (ties resolve to the first), so a Knife (1) is
discarded before a .38\/Machete (2). Assets we have no quality reading for fall
back to 'nonWeaponQuality'. The returned index is into @cs@ (offset 0).
-}
slotDiscardDecision :: AiSituation -> [UI Message] -> Int
slotDiscardDecision sit cs = case sortOn snd scored of
  ((i, _) : _) -> i
  [] -> 0
 where
  scored =
    [ (i, Map.findWithDefault nonWeaponQuality aid (aiAssetQuality sit))
    | (i, TargetLabel (AssetTarget aid) (Discard {} : _)) <- withIndex cs
    ]

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

{- | One token of hedge against the chaos bag: the coarse fallback used only when
the bag is unknown (no scenario) — we then press /Start/ once our committed total
is at least @difficulty + 'commitBuffer'@.
-}
commitBuffer :: Int
commitBuffer = 1

{- | The pass-probability at or above which a test is "good enough": we bank it
(press /Start/, keeping key cards) rather than committing more, and an assister
declines to spend a card on it. Mirrors 'highOddsThreshold' — "if it can get
75%+ without committing, weight that highly".
-}
commitOddsThreshold :: Double
commitOddsThreshold = 0.75

{- | If @q@ is the skill-test commit window (a 'ChooseOne' containing a
'StartSkillTestButton'), decide between committing and starting; otherwise
'Nothing' so generic scoring runs. The returned index is into the
'ChooseOne' choices (offset 0), preserving the index invariant.
-}
commitWindowDecision :: AiSituation -> Question Message -> Maybe Int
commitWindowDecision sit = \case
  ChooseOne cs | any isStartSkillTestButton cs -> Just (decideCommit (aiBagOutcomes sit) (aiSkillTest sit) cs)
  _ -> Nothing

{- | Commit the highest-icon hand card until the test is good enough, then press
Start. Never uncommits.

"Good enough" is odds-based when the bag is known: if our committed total already
gives at least 'commitOddsThreshold' P(success) over a single draw we bank the
test (press Start, saving cards). When the bag is unknown (no scenario, empty
'aiBagOutcomes') we fall back to the old @total >= difficulty + 'commitBuffer'@
hedge. Below the bar we commit the hand card adding the most matching icons.

Terminates because every commit either raises the committed total or, once
nothing committable adds a matching icon, falls through to Start (and the choice
list shrinks with each commit anyway).
-}
decideCommit :: [TokenOutcome] -> Maybe SkillTestInfo -> [UI Message] -> Int
decideCommit outcomes mSti cs = case mSti of
  Nothing -> startIdx
  Just sti -> case stiDifficulty sti of
    Nothing -> startIdx
    Just d
      -- Odds known and already at/over the bar (else the coarse buffer rule):
      -- bank the test and keep key cards. The buffer is also a FLOOR in the
      -- odds case: once the committed total clears difficulty by 'commitBuffer'
      -- we stop, so a test whose ceiling is below 'commitOddsThreshold' (a hard
      -- bag) banks a reasonable margin instead of dumping the whole hand chasing
      -- an unreachable 75%.
      | maybe
          (stiCurrent sti >= d + commitBuffer)
          (\p -> p >= commitOddsThreshold || stiCurrent sti >= d + commitBuffer)
          (passOdds outcomes (stiCurrent sti) d) ->
          startIdx
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

{- | The generic argmax ranking over the selectable choices, returning an index
into @cs@. This is the ONLY ranking the learned model replaces: when
'activeLinearModel' is a 'Just' (the @ARKHAM_AI_USE_MODEL@ flag is on AND a
non-empty model is embedded) the seat ranks by 'learnedScore' instead of the
hand-tuned 'scoreChoice'; otherwise (the default) it runs the heuristic path
below unchanged, byte-for-byte. Either way it ranks over the SAME
@isSelectable@ choices and falls back to index @0@ when none is selectable, so
the engine index invariant and always-legal fallback hold identically. The
special-case handlers (commit window, damage\/slot-discard, amounts) live in
'buildAnswer' \/ 'chooseIndexFor' and are untouched by the model.
-}
bestByScore :: AiSituation -> [UI Message] -> Int
bestByScore sit cs = case activeLinearModel of
  Just m -> argmaxFirst [(i, learnedScore m sit ui) | (i, ui) <- selectable]
  Nothing -> case maxesBy snd scored of
    ((i, _) : _) -> i
    [] -> 0
 where
  selectable = [(i, ui) | (i, ui) <- withIndex cs, isSelectable ui]
  raw = [(i, ui, scoreChoice sit ui) | (i, ui) <- selectable]
  -- Last-action economy gate: on our final action, don't spend it taking
  -- resources or drawing when a genuinely productive action is available
  -- (scores > the end-turn baseline). Off the last action, or when nothing
  -- productive is on offer, economy keeps its normal value.
  lastActionProductive =
    aiRemainingActions sit <= 1
      && any (\(_, ui, s) -> s > 1 && not (isEconomyChoice ui) && not (uiIsStop ui)) raw
  scored =
    [ (i, if lastActionProductive && isEconomyChoice ui then min s 0 else s)
    | (i, ui, s) <- raw
    ]

{- | Argmax over @(index, score)@ pairs returning the index of the highest score,
ties broken to the lowest index (first in list order) — matching the heuristic
path's @'maxesBy' snd@-then-@head@, whose stable sort keeps the earliest-listed
choice among equal maxima. Returns @0@ for an empty list. A strict left fold (not
the partial ClassyPrelude 'maximum', which is 'Int'-typed via 'maxesBy' anyway
and cannot rank these 'Double' scores).
-}
argmaxFirst :: [(Int, Double)] -> Int
argmaxFirst = \case
  [] -> 0
  (p : ps) -> fst (foldl' keepBetter p ps)
 where
  -- Strict @>@: a later choice must STRICTLY beat the incumbent to win, so the
  -- earliest-listed choice survives a tie (lowest index), as above.
  keepBetter best@(_, bestScore) cur@(_, curScore) =
    if curScore > bestScore then cur else best

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

-- * Odds-to-score mapping (chaos-bag probability → kind-score weight)

{- | The base weight a fully-favourable (P=1) fight\/investigate odds term earns
before the high-odds kicker: a coin-flip (P≈0.5) reads as ~4 and a long shot
(P≈0.15) as ~1, sitting in the same band as the kind bases.
-}
oddsScale :: Int
oddsScale = 8

{- | The (smaller) odds scale used for evading, so a favourable evade stays a
touch under a favourable fight (≤ ~10 vs ≤ ~12), preserving the prior ordering
where fighting edged out evading.
-}
evadeOddsScale :: Int
evadeOddsScale = 6

{- | The pass probability at or above which a test counts as a strong play and
earns 'highOddsKicker' on top of the scaled odds. The user's rule: "if it can get
75%+ without committing, weight that highly."
-}
highOddsThreshold :: Double
highOddsThreshold = 0.75

-- | The flat bonus a high-odds (>= 'highOddsThreshold') test earns on top of the scaled odds.
highOddsKicker :: Int
highOddsKicker = 4

{- | Map known odds to a kind-score weight at a given scale: @round (p * scale)@
plus 'highOddsKicker' once @p >= 'highOddsThreshold'@. Unknown odds ('Nothing',
i.e. an empty bag) fall back to the supplied coarse value, so pre-scenario / no-bag
scoring is unchanged. So at the full 'oddsScale': >=75% reads ~10–12, a coin-flip
~4, a long shot ~1–2.
-}
oddsScoreWith :: Int -> Maybe Double -> Int -> Int
oddsScoreWith scale modds fallback =
  maybe
    fallback
    (\p -> round (p * fromIntegral scale) + if p >= highOddsThreshold then highOddsKicker else 0)
    modds

{- | The shared fight\/investigate odds→score mapper at the full 'oddsScale' (the
brief's @oddsScore@): @maybe fallback (\\p -> round (p * oddsScale) + kicker) modds@.
-}
oddsScore :: Maybe Double -> Int -> Int
oddsScore = oddsScoreWith oddsScale

-- * Scoring helpers & tunables (clue / weapon / doom / safety awareness)

{- | The keep value of a non-weapon asset for the slot-discard ranking: a fixed
mid value that sits at\/above a base weapon so a worse weapon is dropped first,
while a slightly-better weapon outranks it. Weapons are valued by their tagged
per-attack 'abDamage' instead.
-}
nonWeaponQuality :: Int
nonWeaponQuality = 2

{- | The most damage a single asset's tagged weapon Fight ability deals per attack
('abDamage', which already includes the base 1), or 'Nothing' when the asset has
no damage-tagged ability (a non-weapon, or an untagged one).
-}
weaponDamageOf :: Card -> Maybe Int
weaponDamageOf card =
  case [d | Just tag <- [lookupCardTag (toCardCode card)], ab <- Map.elems (ctAbilities tag), Just d <- [abDamage ab]] of
    [] -> Nothing
    ds -> Just (foldl' max 1 ds)

{- | The card a turn-menu play choice would play, or 'Nothing' for a non-play
choice. Mirrors the 'classifyUI' play-card shape so the redundancy penalty sees
the same card.
-}
playedCard :: UI Message -> Maybe Card
playedCard = \case
  TargetLabel (CardIdTarget _) (InitiatePlayCardWithWindows _ card _ _ _ _ : _) -> Just card
  _ -> Nothing

{- | Whether playing @card@ is a redundant or strictly-worse weapon: we already
field a weapon, @card@ has the Weapon trait, and its per-attack damage does not
exceed our best controlled weapon. An untagged weapon is treated as base damage
(so it is considered redundant against any controlled weapon).
-}
isRedundantWeapon :: AiSituation -> Card -> Bool
isRedundantWeapon sit card =
  aiControlsWeapon sit
    && member Weapon (cdCardTraits (toCardDef card))
    && fromMaybe 1 (weaponDamageOf card) <= aiBestWeaponDamage sit

-- | The group already holds enough clues to satisfy the clue-spend objective.
cluesAlreadyGathered :: AiSituation -> Bool
cluesAlreadyGathered sit = maybe False (aiGroupClues sit >=) (aiClueTarget sit)

-- | The current act is a clue-spend objective the group still needs clues for.
objectiveWantsClues :: AiSituation -> Bool
objectiveWantsClues sit = maybe False (aiGroupClues sit <) (aiClueTarget sit)

-- | Whether a choice is an economy action (take resources / draw a card).
isEconomyChoice :: UI Message -> Bool
isEconomyChoice = \case
  ResourceLabel _ _ -> True
  ComponentLabel (InvestigatorDeckComponent _) _ -> True
  _ -> False

{- | How far below ending the turn a redundant weapon play is pushed (well below
0 so any real action, and even @EndTurn@, outranks it).
-}
redundantWeaponPenalty :: Int
redundantWeaponPenalty = -100

{- | How far a pure resource-gain card play (role @"economy"@, e.g. Emergency
Cache) is pushed down when resources are not the bottleneck — below drawing (2–5)
so an idle seat digs for answers instead of hoarding resources it cannot spend.
Consumed by 'idleEconomyPenalty' in 'scoreChoice'.
-}
idleEconomyCardPenalty :: Int
idleEconomyCardPenalty = -10

{- | How hard the weakness draw-risk pushes the draw score down: the draw score
loses @round ('aiWeaknessDrawRisk' * 'weaknessDrawPenaltyScale')@ (floored at 0),
so a deck that is, say, a third weaknesses knocks ~4 off drawing — enough to make
the seat wary of drawing into its own weakness.
-}
weaknessDrawPenaltyScale :: Int
weaknessDrawPenaltyScale = 12

-- | Doom fraction (current doom / agenda threshold) at which objective focus kicks in.
doomPressureThreshold :: Double
doomPressureThreshold = 0.6

{- | The doom-pressure act-push scales with how close the agenda is: an
objective-aligned action earns @round ('aiDoomPressure' * 'doomPressureScale')@,
capped at 'doomPressureBumpCap', so the closer the agenda the stronger the push.
-}
doomPressureScale :: Int
doomPressureScale = 8

-- | The ceiling on the scaled doom-pressure act-push.
doomPressureBumpCap :: Int
doomPressureBumpCap = 10

-- | Remaining-health (or sanity) at or below which self-preservation engages.
lowHealthThreshold :: Int
lowHealthThreshold = 2

{- | The conservative nudge self-preservation applies: it lowers a dangerous
fight and raises an evade by this much.
-}
selfPreservationDelta :: Int
selfPreservationDelta = 4

-- * Teammate-aware coordination (bounded nudges, never vetoes)

{- | How far a Fight is nudged down when a /co-engaged/ teammate can already
finish that enemy soon (so the seat does something else). Modest: a clean
finisher (combat term up to ~28) still wins, and a directive ('aiPrioritySet')
suppresses it entirely. Subtracted in 'scoreChoice'.
-}
teammateHandledPenalty :: Int
teammateHandledPenalty = 6

{- | How far an Investigate is nudged down when a co-located, clearly-better
investigator should take the (one-or-fewer) clue here instead. Subtracted in
'scoreChoice'.
-}
investigateDeferralPenalty :: Int
investigateDeferralPenalty = 10

{- | A small POSITIVE nudge for rescuing a teammate: a Fight on an enemy a
teammate is engaged with but /cannot/ finish themselves, that the seat can
meaningfully damage. Bounded well under a directive so it only re-ranks
already-legal fights. Added in 'scoreChoice'.
-}
rescueBonus :: Int
rescueBonus = 4

{- | Up to how many clues a stronger co-located investigator is assumed able to
clear on their own this turn — at or below this the weaker seat defers the
location to them (a fuller location needs everyone digging). -}
deferClueCoverage :: Int
deferClueCoverage = 3

{- | The small (≤4) specialty bump/malus that divides labor in a mixed party: a
combat-specialty seat leans into fighting (and off investigating) when the party
holds a stronger investigator, and a clue-specialty seat the reverse when it
holds a stronger fighter. Bounded well under the kind\/finisher scores so live
tactical signals still dominate.
-}
specialtyDivisionDelta :: Int
specialtyDivisionDelta = 3

-- | The seat's intrinsic specialty focus (its profile\/deck focus, not the
-- act-blended active focus). Used to divide labor across a mixed party.
seatSpecialty :: AiSituation -> Focus
seatSpecialty sit = profileFocus (aiState sit) (aiStats sit)

{- | Whether a teammate out-investigates the seat: strictly higher modified
intellect, or an investigate deck focus where the seat's own specialty is not
investigate.
-}
outInvestigatesSeat :: AiSituation -> TeammateInfo -> Bool
outInvestigatesSeat sit tm =
  tmIntellect tm > myIntellect
    || (tmDeckFocus tm == Just InvestigateFocus && seatSpecialty sit /= InvestigateFocus)
 where
  myIntellect = maybe 0 (`statsSkillValue` SkillIntellect) (aiStats sit)

{- | Whether some teammate /co-located with the seat/ out-investigates it (nudge
2). 'False' when the seat is off the map, so we never defer a clue to a teammate
we are not actually standing with.
-}
betterInvestigatorColocated :: AiSituation -> Bool
betterInvestigatorColocated sit = case aiLocation sit of
  Nothing -> False
  here -> any (\tm -> tmLocation tm == here && outInvestigatesSeat sit tm) (aiTeammates sit)

-- | Whether the party (anywhere) holds an investigator stronger than the seat.
partyHasStrongerInvestigator :: AiSituation -> Bool
partyHasStrongerInvestigator sit = any (outInvestigatesSeat sit) (aiTeammates sit)

-- | Whether the party (anywhere) holds a fighter with strictly higher combat.
partyHasStrongerFighter :: AiSituation -> Bool
partyHasStrongerFighter sit = any ((> myCombat) . tmCombat) (aiTeammates sit)
 where
  myCombat = maybe 0 (`statsSkillValue` SkillCombat) (aiStats sit)

{- | Whether some teammate /engaged with this enemy/ can defeat it within roughly
two turns: their combat clears its fight value (an unknown fight is treated as
reachable) and twice their best per-attack weapon damage covers its remaining
health. 'False' for an enemy with no damage-killable health (so we never treat a
non-damage enemy as "handled"), and 'False' when no teammate is engaged with it
(a teammate engaged but unable to finish it is /not/ counted, since helping is
good). Used by nudge 1.
-}
teammateCanHandleEnemy :: AiSituation -> EnemyId -> Bool
teammateCanHandleEnemy sit eid = fromMaybe False $ do
  info <- Map.lookup eid (aiEnemies sit)
  rh <- eiRemainingHealth info
  pure $ any (canHandle (eiFight info) rh) (aiTeammates sit)
 where
  canHandle mFight rh tm =
    eid `elem` tmEngaged tm
      && maybe True (tmCombat tm >=) mFight
      && tmWeaponDamage tm * 2 >= rh

{- | Whether some teammate is engaged with this enemy but the party engaged with
it /cannot/ finish it soon (the inverse of 'teammateCanHandleEnemy', given an
engaged teammate). Marks an enemy a teammate is stuck on and would welcome help
clearing. Used by the rescue nudge.
-}
teammateEngagedButStuck :: AiSituation -> EnemyId -> Bool
teammateEngagedButStuck sit eid =
  any (\tm -> eid `elem` tmEngaged tm) (aiTeammates sit)
    && not (teammateCanHandleEnemy sit eid)

{- | Whether the seat can meaningfully damage this enemy: it has damage-killable
health and the seat's modified combat clears its fight value (an unknown fight is
treated as reachable; the seat's weapon always deals at least the unarmed 1).
-}
seatCanDamageEnemy :: AiSituation -> EnemyId -> Bool
seatCanDamageEnemy sit eid = fromMaybe False $ do
  info <- Map.lookup eid (aiEnemies sit)
  _ <- eiRemainingHealth info -- damage-killable only
  let myCombat = maybe 0 (`statsSkillValue` SkillCombat) (aiStats sit)
  pure (maybe True (myCombat >=) (eiFight info))

{- | Score a single choice for the current situation; higher is more attractive.
Pure and deterministic so it can be unit-tested with synthetic choices. The
weights are coarse v1 heuristics to be tuned with play data, not a solved
policy.

The numeric output is, /by construction/, the sum of the named additive terms
'scoreBreakdown' exposes — so the two never diverge and the scorer the AI runs
is exactly the scorer the ML pipeline distils against. Behaviour is unchanged
from the prior single-expression definition (the same ten terms, same order).
-}
scoreChoice :: AiSituation -> UI Message -> Int
scoreChoice sit ui = sum (map snd (scoreBreakdown sit ui))

{- | The ten named additive terms 'scoreChoice' sums, in scoring order:
@priorityScore@, @kindScore@, @stopScore@, @locationScore@, @economyScore@,
@redundancyPenalty@, @idleEconomyPenalty@, @doomBump@, @safetyAdjust@,
@teammateAdjust@.

This is the single source of truth for the score: @scoreChoice sit ui == sum
(map snd (scoreBreakdown sit ui))@. It exists so the same per-choice
sub-computations feed three consumers without divergence — the live scorer
('scoreChoice' \/ 'bestByScore'), the coarse interpretable features in the
ML vector ('choiceFeatures'), and a linear-distillation target. The key set
and order are stable (always these ten labels), so it can back a dataset
column set directly.
-}
scoreBreakdown :: AiSituation -> UI Message -> [(Text, Int)]
scoreBreakdown sit ui =
  [ ("priorityScore", priorityScore)
  , ("kindScore", kindScore)
  , ("stopScore", stopScore)
  , ("locationScore", locationScore)
  , ("economyScore", economyScore)
  , ("redundancyPenalty", redundancyPenalty)
  , ("idleEconomyPenalty", idleEconomyPenalty)
  , ("doomBump", doomBump)
  , ("safetyAdjust", safetyAdjust)
  , ("teammateAdjust", teammateAdjust)
  ]
 where
  skill s = (\st -> statsSkillValue st s) <$> aiStats sit
  kind = classifyUI ui
  -- Proportional focus term (replaces the old binary align bonus): a choice's
  -- focus earns up to 'focusBonusCap', scaled by how heavily that focus is
  -- weighted in the blend.
  focusBonus = focusWeightBonus (aiFocusWeights sit)
  -- The agenda is closing in: lean into the act objective, drop economy.
  underDoomPressure = aiDoomPressure sit >= doomPressureThreshold

  priorityScore = case uiTarget ui of
    Just t | t `elem` aiPrioritySet sit -> priorityBonus
    _ -> 0

  -- Economy as setup. Taking resources is worth saving for only when there is a
  -- concrete aligned card we cannot yet afford; drawing digs for a missing focus
  -- card (5), is mild card advantage otherwise (2), and is never worth filling
  -- the hand into a discard (0). Both stay <= 6 so any viable productive action
  -- (8+) outranks them, and both beat ending the turn (1) when they make
  -- progress. Under doom pressure economy is suppressed entirely.
  economyScore
    | underDoomPressure = 0
    | otherwise = case ui of
        ResourceLabel _ _ -> if aiResourceShortfall sit > 0 then 6 else 0
        -- Drawing also weighs the weakness draw-risk: the closer the deck is to a
        -- weakness on top, the lower the draw score (floored at 0).
        ComponentLabel (InvestigatorDeckComponent _) _
          | not (aiHandNotFull sit) -> 0
          | aiShouldDig sit -> max 0 (5 - weaknessDrawPenalty)
          | otherwise -> max 0 (2 - weaknessDrawPenalty)
        _ -> 0
  weaknessDrawPenalty = round (aiWeaknessDrawRisk sit * fromIntegral weaknessDrawPenaltyScale)

  -- Don't trade down weapons (area 2): a Weapon hand-card whose tagged per-attack
  -- damage does not beat our best controlled weapon, while we already field one,
  -- is pushed below ending the turn so the seat keeps the .38 instead of playing
  -- a Knife. A strictly better weapon (more damage) is an upgrade and untouched.
  redundancyPenalty = case playedCard ui of
    Just card | isRedundantWeapon sit card -> redundantWeaponPenalty
    _ -> 0

  -- Don't burn a card on resources we don't need: a pure resource-gain card
  -- (role @"economy"@, e.g. Emergency Cache) is pushed below drawing when there
  -- is no concrete unaffordable aligned card (@aiResourceShortfall <= 0@), so an
  -- idle seat digs for answers instead of hoarding.
  idleEconomyPenalty = case playedCard ui of
    Just card
      | aiResourceShortfall sit <= 0
      , Just tag <- lookupCardTag (toCardCode card)
      , ctRole tag == Just "economy" ->
          idleEconomyCardPenalty
    _ -> 0

  -- Doom pressure (area 3): bump fighting the act's defeat-enemy target and
  -- gathering its clues, so the seat closes out the objective before the agenda.
  doomBump
    | not underDoomPressure = 0
    | otherwise = case kind of
        FightChoice (Just eid) | Just eid == aiObjectiveEnemy sit -> scaledDoomBump
        InvestigateChoice | objectiveWantsClues sit -> scaledDoomBump
        _ -> 0
  -- Scale the act-push with how close the agenda is, capped at
  -- 'doomPressureBumpCap' so it never eclipses a finisher or a directive.
  scaledDoomBump = min doomPressureBumpCap (round (aiDoomPressure sit * fromIntegral doomPressureScale))

  -- Self-preservation (area 3): at low remaining health, ease off attacking a
  -- dangerous enemy we cannot finish; favour slipping away when health OR sanity
  -- is low. Conservative — a clean finisher's combat bonus still wins.
  safetyAdjust = case kind of
    FightChoice (Just eid) | lowHealth && isDangerous eid -> negate selfPreservationDelta
    EvadeChoice _ | lowVitality -> selfPreservationDelta
    _ -> 0
   where
    lowHealth = aiRemainingHealth sit <= lowHealthThreshold
    lowVitality = lowHealth || aiRemainingSanity sit <= lowHealthThreshold
    -- Dangerous: an enemy we are engaged with whose attack could defeat us and
    -- that we cannot one-shot this attack (so fighting it leaves us exposed).
    isDangerous eid = fromMaybe False $ do
      info <- Map.lookup eid (aiEnemies sit)
      let engagedHere = eid `elem` aiEngaged sit
          couldDefeatUs = eiAttackDamage info >= aiRemainingHealth sit
          cannotFinish = maybe True (> aiBestWeaponDamage sit) (eiRemainingHealth info)
      pure (engagedHere && couldDefeatUs && cannotFinish)

  -- Teammate-aware coordination: three bounded nudges that divide labor across
  -- the party. ALL are gated on there being teammates, so with an empty
  -- 'aiTeammates' (solo / single-AI game) this term is exactly 0 and the policy
  -- is byte-for-byte the pre-teammate behavior. None of them can resurrect an
  -- illegal action or beat a directive — they only re-rank already-legal choices.
  teammateAdjust
    | null (aiTeammates sit) = 0
    | otherwise = handledOff + investigateDefer + specialtyDivide + rescueHelp

  -- (1) Don't pile onto an enemy a co-engaged teammate can already finish soon,
  -- UNLESS it is a directive target (a priority overrides). A teammate engaged
  -- but unable to finish it is not counted, so genuinely helping is never
  -- penalized.
  handledOff = case kind of
    FightChoice (Just eid)
      | EnemyTarget eid `notElem` aiPrioritySet sit
      , teammateCanHandleEnemy sit eid ->
          negate teammateHandledPenalty
    _ -> 0

  -- (2) Role-aware investigate deferral: when a co-located, clearly-better
  -- investigator is here and they can plausibly clear the clues themselves
  -- ('deferClueCoverage'), let them take the location and do something else —
  -- a weak-intellect combat seat shouldn't grind marginal-odds investigates next
  -- to a seeker. The strongest investigator present is unaffected, and a clue-rich
  -- location (above the coverage cap) still wants everyone digging.
  investigateDefer = case kind of
    InvestigateChoice
      | aiLocationClues sit <= deferClueCoverage
      , betterInvestigatorColocated sit ->
          negate investigateDeferralPenalty
    _ -> 0

  -- (3) Specialty division of labor: a combat-specialty seat leans into fighting
  -- and off investigating when the party fields a stronger investigator; a
  -- clue-specialty seat does the reverse when the party fields a stronger
  -- fighter. Kept to ±'specialtyDivisionDelta' so tactical signals dominate.
  specialtyDivide = case seatSpecialty sit of
    CombatFocus
      | partyHasStrongerInvestigator sit -> case kind of
          FightChoice _ -> specialtyDivisionDelta
          InvestigateChoice -> negate specialtyDivisionDelta
          _ -> 0
    InvestigateFocus
      | partyHasStrongerFighter sit -> case kind of
          InvestigateChoice -> specialtyDivisionDelta
          FightChoice _ -> negate specialtyDivisionDelta
          _ -> 0
    _ -> 0

  -- (4) Rescue reward: a Fight on an enemy a teammate is engaged with but cannot
  -- finish themselves, that the seat can meaningfully damage. The existing
  -- 'handledOff' only penalizes piling onto an enemy a teammate CAN finish; this
  -- rewards the opposite — going to help where a teammate is stuck. Bounded to
  -- +'rescueBonus' so it only re-ranks legal fights and never overrides a directive.
  rescueHelp = case kind of
    FightChoice (Just eid)
      | teammateEngagedButStuck sit eid
      , seatCanDamageEnemy sit eid ->
          rescueBonus
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

  kindScore = case kind of
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
          (Just c, Just f) ->
            -- Odds-based to-hit; falls back to the prior coarse c>=f ? 8 : c+1>=f ? 4 : 0
            -- when the bag is empty (no scenario). The kill-range 'finish' is unchanged.
            let toHitFallback
                  | c >= f = 8
                  | c + 1 >= f = 4
                  | otherwise = 0
             in oddsScore (passOdds (aiBagOutcomes sit) c f) toHitFallback
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
      (Just a, Just e) ->
        -- Odds-based at the smaller 'evadeOddsScale' so evade stays a touch under
        -- fight; falls back to the prior flat 6 (for a>=e) when the bag is empty.
        pure (oddsScoreWith evadeOddsScale (passOdds (aiBagOutcomes sit) a e) (if a >= e then 6 else 0))
      _ -> pure 0

  -- Only an action-investigate is gated on there being clues here; reaction
  -- abilities that grant clues are handled as 'FocusedChoice' (see classifyUI).
  -- Once the group already holds enough clues for the spend objective, stop
  -- over-investigating (the goal is now to be at the spend location).
  investigateBonus
    | cluesAlreadyGathered sit = 0
    | aiLocationClues sit <= 0 = 0
    | otherwise = case (skill SkillIntellect, aiLocationShroud sit) of
        (Just i, Just sh) ->
          -- Odds-based; falls back to the prior coarse i>=sh ? 8 : 3 when the bag is empty.
          oddsScore (passOdds (aiBagOutcomes sit) i sh) (if i >= sh then 8 else 3)
        _ -> 4

-- * ML feature extraction (shared by training-data extraction and inference)

{- | A stable, lowercase key fragment for a 'Focus', used in feature names. Kept
deliberately /independent/ of the JSON wire key ('Arkham.Ai.Focus.focusKey') so
the ML schema can never silently drift if the on-wire format is ever renamed:
this mapping is owned by the feature schema and must stay fixed. The values
happen to coincide with the wire key today; that is a convenience, not a
coupling.
-}
focusFeatureName :: Focus -> Text
focusFeatureName = \case
  CombatFocus -> "combat"
  InvestigateFocus -> "investigate"
  EvadeFocus -> "evade"
  SupportFocus -> "support"
  SurvivalFocus -> "survival"
  MobilityFocus -> "mobility"

{- | A stable, lowercase key fragment for a 'ChoiceKind', used in the @ch.kind.*@
one-hot. Total over the seven constructors.
-}
choiceKindName :: ChoiceKind -> Text
choiceKindName = \case
  FightChoice _ -> "fight"
  EvadeChoice _ -> "evade"
  InvestigateChoice -> "investigate"
  MoveChoice -> "move"
  PlayCardChoice _ -> "playCard"
  FocusedChoice _ -> "focused"
  PlainChoice -> "plain"

{- | All 'ChoiceKind' key fragments in the fixed one-hot order used by
'choiceFeatures' (constructor order). The full key set is locked, so this list
must not be reordered.
-}
allChoiceKindNames :: [Text]
allChoiceKindNames = ["fight", "evade", "investigate", "move", "playCard", "focused", "plain"]

-- | 0\/1 indicator as a 'Double', for one-hot and boolean features.
indicator :: Bool -> Double
indicator b = if b then 1 else 0

{- | Chaos-bag summary over the resolved 'aiBagOutcomes': the fraction that are
'OutcomeFail', the fraction that are 'OutcomeSucceed', and the mean of the
'OutcomeMod' values (mean taken over the modifier tokens only). All three are 0
when the bag is empty, and the mean is 0 when there are no modifier tokens. Uses
strict 'foldl''-style sums, never the partial ClassyPrelude 'maximum'\/'minimum'.
-}
bagOutcomeSummary :: [TokenOutcome] -> (Double, Double, Double)
bagOutcomeSummary outcomes =
  let total = length outcomes
      frac n = if total == 0 then 0 else fromIntegral n / fromIntegral total
      nFail = foldl' (\acc o -> if o == OutcomeFail then acc + 1 else acc) (0 :: Int) outcomes
      nSucceed = foldl' (\acc o -> if o == OutcomeSucceed then acc + 1 else acc) (0 :: Int) outcomes
      mods = [m | OutcomeMod m <- outcomes]
      meanMod = case mods of
        [] -> 0
        _ -> fromIntegral (foldl' (+) (0 :: Int) mods) / fromIntegral (length mods)
   in (frac nFail, frac nSucceed, meanMod)

{- | A dense, stably-keyed feature vector for one @(situation, choice)@ pair,
used /identically/ at training-data extraction and at inference, so there is no
train\/serve skew — both call this one function, and the @ch.*@ block is derived
through the very helpers 'scoreChoice' uses ('classifyUI', 'passOdds',
'isEconomyChoice', 'uiIsStop', 'isRedundantWeapon', 'uiTarget', 'playedCard'), so
a feature and the score it explains can never diverge.

EVERY key is emitted for EVERY choice — features that do not apply to a choice's
kind are 0 — so the column set is fixed and the dataset stays rectangular.
Ordering is stable. Two greppable blocks:

  * @sit.*@ — read straight off the 'AiSituation': the 'aiActiveFocus' one-hot,
    the six 'aiFocusWeights', the scalar situation reads, the four 'aiStats'
    skills, the chaos-bag summary, and enemy\/teammate aggregates.
  * @ch.*@ — the 'ChoiceKind' one-hot plus per-choice flags and odds.
-}
{- | Score a choice with the distilled standardized linear model (see
'Arkham.Ai.Model'). For each feature 'choiceFeatures' emits, standardize its
value @(v - mu) \/ sd@ and weight it by the model coefficient, summing only the
features the model actually weights (@coef \/= 0@). Missing @mu@\/@sd@ default to
@0@\/@1@ and @sd@ is floored at @1e-9@ to avoid a divide-by-zero on a degenerate
(constant) feature. Reuses 'choiceFeatures' verbatim so training and inference
see identical features; 'scoreChoice' \/ 'scoreBreakdown' are untouched.
-}
learnedScore :: LinearModel -> AiSituation -> UI Message -> Double
learnedScore m sit ui =
  sum
    [ ((v - mu) / sd) * coef
    | (k, v) <- choiceFeatures sit ui
    , let coef = Map.findWithDefault 0 k (lmCoef m)
    , coef /= 0
    , let mu = Map.findWithDefault 0 k (lmMu m)
    , let sd = max 1e-9 (Map.findWithDefault 1 k (lmSd m))
    ]

{- | The interpretable per-(situation, choice) feature vector (67 stable keys):
the situation block ('situationFeatures') concatenated with the per-choice block
('choiceFeaturesBlock'). The single source of truth for BOTH the offline trainer
(@ml/train.py@) and the inference-time 'learnedScore', so the distilled model
sees exactly the features it was trained on.
-}
choiceFeatures :: AiSituation -> UI Message -> [(Text, Double)]
choiceFeatures sit ui = situationFeatures <> choiceFeaturesBlock
 where
  seatSkillValue s = maybe 0 (`statsSkillValue` s) (aiStats sit)
  (bagFail, bagSucceed, bagMeanMod) = bagOutcomeSummary (aiBagOutcomes sit)
  enemyInfos = Map.elems (aiEnemies sit)
  enemyHealths = mapMaybe eiRemainingHealth enemyInfos
  minEnemyHealth = case enemyHealths of
    [] -> 0
    (x : xs) -> foldl' min x xs
  myIntellect = seatSkillValue SkillIntellect

  situationFeatures =
    [ ("sit.focus." <> focusFeatureName f, indicator (aiActiveFocus sit == f))
    | f <- allFoci
    ]
      <> [ ("sit.weight." <> focusFeatureName f, fromIntegral (Map.findWithDefault 0 f (aiFocusWeights sit)))
         | f <- allFoci
         ]
      <> [ ("sit.resources", fromIntegral (aiResources sit))
         , ("sit.remainingActions", fromIntegral (aiRemainingActions sit))
         , ("sit.locationClues", fromIntegral (aiLocationClues sit))
         , ("sit.locationShroud", fromIntegral (fromMaybe 0 (aiLocationShroud sit)))
         , ("sit.doomPressure", aiDoomPressure sit)
         , ("sit.remainingHealth", fromIntegral (aiRemainingHealth sit))
         , ("sit.remainingSanity", fromIntegral (aiRemainingSanity sit))
         , ("sit.groupClues", fromIntegral (aiGroupClues sit))
         , ("sit.clueTarget", fromIntegral (fromMaybe 0 (aiClueTarget sit)))
         , ("sit.hasClueTarget", indicator (isJust (aiClueTarget sit)))
         , ("sit.resourceShortfall", fromIntegral (aiResourceShortfall sit))
         , ("sit.weaknessDrawRisk", aiWeaknessDrawRisk sit)
         , ("sit.bestWeaponDamage", fromIntegral (aiBestWeaponDamage sit))
         , ("sit.shouldDig", indicator (aiShouldDig sit))
         , ("sit.handNotFull", indicator (aiHandNotFull sit))
         , ("sit.controlsWeapon", indicator (aiControlsWeapon sit))
         , ("sit.stat.willpower", fromIntegral (seatSkillValue SkillWillpower))
         , ("sit.stat.intellect", fromIntegral (seatSkillValue SkillIntellect))
         , ("sit.stat.combat", fromIntegral (seatSkillValue SkillCombat))
         , ("sit.stat.agility", fromIntegral (seatSkillValue SkillAgility))
         , ("sit.bagFail", bagFail)
         , ("sit.bagSucceed", bagSucceed)
         , ("sit.bagMeanMod", bagMeanMod)
         , ("sit.nEngaged", fromIntegral (length (aiEngaged sit)))
         , ("sit.nEnemies", fromIntegral (Map.size (aiEnemies sit)))
         , ("sit.minEnemyRemainingHealth", fromIntegral minEnemyHealth)
         , ("sit.anyInKillRange", indicator (any (maybe False (<= aiBestWeaponDamage sit) . eiRemainingHealth) enemyInfos))
         , ("sit.nTeammates", fromIntegral (length (aiTeammates sit)))
         , ("sit.maxTmCombat", fromIntegral (foldl' max 0 (map tmCombat (aiTeammates sit))))
         , ("sit.maxTmIntellect", fromIntegral (foldl' max 0 (map tmIntellect (aiTeammates sit))))
         , ("sit.strongerInvestigator", indicator (any ((> myIntellect) . tmIntellect) (aiTeammates sit)))
         ]

  kind = classifyUI ui
  mTarget = uiTarget ui

  isMoveTarget = case mTarget of
    Just (LocationTarget lid) -> Just lid == aiMoveTarget sit
    _ -> False

  targetDistance = case mTarget of
    Just (LocationTarget lid) -> fromIntegral (Map.findWithDefault 99 lid (aiLocationDistances sit))
    _ -> 99

  fightOdds = case kind of
    FightChoice (Just eid) -> case Map.lookup eid (aiEnemies sit) of
      Just info -> maybe 0 (\f -> fromMaybe 0 (passOdds (aiBagOutcomes sit) (seatSkillValue SkillCombat) f)) (eiFight info)
      Nothing -> 0
    _ -> 0

  enemyRemainingHealth = case kind of
    FightChoice (Just eid) -> maybe 0 (fromIntegral . fromMaybe 0 . eiRemainingHealth) (Map.lookup eid (aiEnemies sit))
    _ -> 0

  isFinisher = case kind of
    FightChoice (Just eid) ->
      indicator $
        maybe
          False
          (maybe False (<= aiBestWeaponDamage sit) . eiRemainingHealth)
          (Map.lookup eid (aiEnemies sit))
    _ -> 0

  evadeOdds = case kind of
    EvadeChoice (Just eid) -> case Map.lookup eid (aiEnemies sit) of
      Just info -> maybe 0 (\e -> fromMaybe 0 (passOdds (aiBagOutcomes sit) (seatSkillValue SkillAgility) e)) (eiEvade info)
      Nothing -> 0
    _ -> 0

  investigateOdds = case kind of
    InvestigateChoice ->
      maybe 0 (\sh -> fromMaybe 0 (passOdds (aiBagOutcomes sit) (seatSkillValue SkillIntellect) sh)) (aiLocationShroud sit)
    _ -> 0

  playFocus = case kind of
    PlayCardChoice mf -> mf
    _ -> Nothing

  choiceFeaturesBlock =
    [ ("ch.kind." <> name, indicator (choiceKindName kind == name))
    | name <- allChoiceKindNames
    ]
      <> [ ("ch.isPriorityTarget", indicator (maybe False (`elem` aiPrioritySet sit) mTarget))
         , ("ch.isEconomy", indicator (isEconomyChoice ui))
         , ("ch.isStop", indicator (uiIsStop ui))
         , ("ch.isRedundantWeapon", indicator (maybe False (isRedundantWeapon sit) (playedCard ui)))
         , ("ch.isMoveTarget", indicator isMoveTarget)
         , ("ch.targetDistance", targetDistance)
         , ("ch.fightOdds", fightOdds)
         , ("ch.enemyRemainingHealth", enemyRemainingHealth)
         , ("ch.isFinisher", isFinisher)
         , ("ch.evadeOdds", evadeOdds)
         , ("ch.investigateOdds", investigateOdds)
         ]
      <> [ ("ch.playFocus." <> focusFeatureName f, indicator (playFocus == Just f))
         | f <- allFoci
         ]

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
