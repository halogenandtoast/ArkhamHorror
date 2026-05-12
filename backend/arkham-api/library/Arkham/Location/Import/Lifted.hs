module Arkham.Location.Import.Lifted (module X, module Arkham.Location.Import.Lifted) where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Helpers.Message as X (pattern R1, pattern R2)
import Arkham.Helpers.Modifiers as X (toModifiers)
import Arkham.Id as X
import Arkham.Location.Helpers as X (adjacentLocations, connectsToAdjacent)
import Arkham.Message as X (
  pattern AfterEvadeEnemy,
  pattern AttackEnemy,
  pattern ChooseEvadeEnemy,
  pattern ChooseFightEnemy,
  pattern ChosenEvadeEnemy,
  pattern EvadeEnemy,
  pattern FailedAttackEnemy,
  pattern FightEnemy,
  pattern TryEvadeEnemy,
  pattern AfterEnemyAttack,
  pattern ChangeEnemyAttackDetails,
  pattern ChangeEnemyAttackTarget,
  pattern EnemiesAttack,
  pattern EnemyAttack,
  pattern EnemyAttackFromDiscard,
  pattern EnemyAttackIfEngaged,
  pattern EnemyWillAttack,
  pattern InitiateEnemyAttack,
  pattern PerformEnemyAttack,
  pattern AssetDefeated,
  pattern CheckDefeated,
  pattern DefeatEnemy,
  pattern EnemyLocationDefeated,
  pattern Exhaust,
  pattern HealAllDamage,
  pattern HealDamage,
  pattern DealDamage,
  pattern Damaged,
  pattern Defeated,
  pattern PlaceAdditionalDamage,
  pattern Ready,
  pattern ReadyAlternative,
  pattern ReadyExhausted,
  pattern CheckEnemyEngagement,
  pattern ChooseEngageEnemy,
  pattern DisengageEnemy,
  pattern DisengageEnemyFromAll,
  pattern EnemyCheckEngagement,
  pattern EnemyEngageInvestigator,
  pattern EngageEnemy,
  pattern EnemyEntered,
  pattern EnemySpawn,
  pattern EnemySpawnAtLocationMatching,
  pattern EnemySpawnEngagedWith,
  pattern EnemySpawnEngagedWithPrey,
  pattern EnemySpawnFromOutOfPlay,
  pattern EnemySpawned,
  pattern EnemyMove,
  pattern HandleElusive,
  pattern HunterMove,
  pattern HuntersMove,
  pattern MoveToward,
  pattern MoveUntil,
  pattern PatrolMove,
  pattern WillMoveEnemy,
  pattern FlipClues,
  pattern MoveAllCluesTo,
  pattern PlaceCluesUpToClueValue,
  pattern RemoveAllClues,
  pattern FlipDoom,
  pattern RemoveAllDoom,
  pattern RemoveAllDoomFromPlay,
  pattern ClearTokens,
  pattern MoveTokens,
  pattern MoveTokensNoDefeated,
  pattern PlaceTokens,
  pattern RemoveAllTokens,
  pattern RemoveTokens,
  pattern RemoveAllChaosTokens,
  pattern SealChaosToken,
  pattern SealedChaosToken,
  pattern SetChaosTokenAside,
  pattern UnsealChaosToken,
  pattern CancelAssetHorror,
  pattern CancelHorror,
  pattern ExcessHealHorror,
  pattern HealAllHorror,
  pattern HealHorror,
  pattern AbilityIsSkillTest,
  pattern CollectSkillTestOptions,
  pattern NextSkillTest,
  pattern BeforeSkillTest,
  pattern ChangeSkillTestType,
  pattern IncreaseSkillTestDifficulty,
  pattern ReplaceSkillTestSkill,
  pattern RepeatSkillTest,
  pattern SetSkillTestTarget,
  pattern SetSkillTestResolveFailureInvestigator,
  pattern BeginSkillTestWithPreMessages,
  pattern BeginSkillTestWithPreMessages',
  pattern BeginSkillTestAfterFast,
  pattern CommitCard,
  pattern CommitToSkillTest,
  pattern FailSkillTest,
  pattern FailedSkillTest,
  pattern PassSkillTest,
  pattern PassSkillTestBy,
  pattern PassedSkillTest,
  pattern RerunSkillTest,
  pattern RevelationSkillTest,
  pattern RunSkillTest,
  pattern AfterThisTestResolves,
  pattern StartSkillTest,
  pattern Successful,
  pattern Failed,
  pattern TriggerSkillTest,
  pattern SkillTestResultOption,
  pattern SkillTestResultOptions,
  pattern RecalculateSkillTestResults,
  pattern RecalculateSkillTestResultsCanChangeAutomatic,
  pattern SkillTestApplyResults,
  pattern SkillTestApplyResultsAfter,
  pattern SkillTestAsk,
  pattern SkillTestCommitCard,
  pattern SkillTestEnds,
  pattern SkillTestEnded,
  pattern AfterSkillTestEnds,
  pattern SkillTestResults,
  pattern SkillTestUncommitCard,
  pattern AfterSkillTestQuiet,
  pattern AfterSkillTestOption,
  pattern EndSkillTestWindow,
  pattern ReturnSkillTestRevealedChaosTokens,
  pattern RevealSkillTestChaosTokens,
  pattern RevealSkillTestChaosTokensAgain,
  pattern DrawChaosToken,
  pattern ResolveChaosToken,
  pattern TargetResolveChaosToken,
  pattern RevealChaosToken,
  pattern SilentRevealChaosToken,
  pattern SwapChaosToken,
  pattern RemoveChaosToken,
  pattern ResetTokenPool,
  pattern RequestChaosTokens,
  pattern RequestedChaosTokens,
  pattern ReturnChaosTokens,
  pattern ReturnChaosTokensToPool,
  pattern RunBag,
  pattern RunDrawFromBag,
  pattern FinalizeRequestedChaosTokens,
  pattern NextChaosBagStep,
  pattern ChooseChaosTokenGroups,
  pattern BeforeRevealChaosTokens,
  pattern AfterRevealChaosTokens,
  pattern ReplaceCurrentDraw,
  pattern ReplaceEntireDraw,
  pattern SetChaosBagChoice,
  pattern ResetChaosTokens,
  pattern ChaosTokenSelected,
  pattern ChaosTokenIgnored,
  pattern ChaosTokenCanceled,
  pattern ForceChaosTokenDraw,
  pattern ForceChaosTokenDrawToken,
  pattern SetChaosTokens,
  pattern SetChaosTokensForScenario,
  pattern ObtainChaosToken,
  pattern Search,
  pattern ResolveSearch,
  pattern FinishedSearch,
  pattern PreSearchFound,
  pattern SearchFound,
  pattern FoundCards,
  pattern SearchNoneFound,
  pattern UpdateSearchReturnStrategy,
  pattern SearchCollectionForRandom,
  pattern SearchEnded,
  pattern CancelSearch,
  pattern ClearFound,
  pattern RemoveAsset,
  pattern RemoveEnemy,
  pattern RemoveEvent,
  pattern RemoveSkill,
  pattern RemoveTreachery,
  pattern RemoveLocation,
  pattern InvestigatorAssignDamage,
  pattern InvestigatorCommittedCard,
  pattern InvestigatorCommittedSkill,
  pattern InvestigatorDamage,
  pattern InvestigatorDamageEnemy,
  pattern InvestigatorDamageInvestigator,
  pattern InvestigatorDefeated,
  pattern InvestigatorIsDefeated,
  pattern InvestigatorDirectDamage,
  pattern InvestigatorDiscardAllClues,
  pattern InvestigatorDoAssignDamage,
  pattern InvestigatorDrawEnemy,
  pattern InvestigatorDrewEncounterCard,
  pattern InvestigatorDrewEncounterCardFrom,
  pattern InvestigatorDrewPlayerCardFrom,
  pattern InvestigatorEliminated,
  pattern InvestigatorKilled,
  pattern InvestigatorMulligan,
  pattern InvestigatorsMulligan,
  pattern InvestigatorPlaceAllCluesOnLocation,
  pattern InvestigatorPlaceCluesOnLocation,
  pattern InvestigatorPlayAsset,
  pattern InvestigatorClearUnusedAssetSlots,
  pattern InvestigatorAdjustAssetSlots,
  pattern InvestigatorAdjustSlot,
  pattern InvestigatorPlayedAsset,
  pattern InvestigatorPlayEvent,
  pattern InvestigatorResigned,
  pattern InvestigatorSpendClues,
  pattern InvestigatorWhenDefeated,
  pattern InvestigatorWhenEliminated,
  pattern InvestigatorSpecific,
  pattern HandleKilledOrInsaneInvestigators,
  pattern Resign,
  pattern ResignWith,
  pattern Devour,
  pattern Devoured,
  pattern BeforePlayEvent,
  pattern CreatePendingEvent,
  pattern BeforeCardCost,
  pattern FinishedEvent,
 )
-- Note: pattern EnemyEvaded is intentionally NOT re-exported here, because
-- Location cards use the matcher form `EnemyEvaded #after You …` from
-- Arkham.Matcher (also imported by this module). Location cards that need
-- the message pattern can import it directly from Arkham.Message.
import Arkham.Location.Runner as X (
  IsLocation,
  LocationAttrs (..),
  LocationCard,
  Message (..),
  canBeFlippedL,
  cardsUnderneathL,
  connectedMatchersL,
  connectsToL,
  costToEnterUnrevealedL,
  extendRevealed,
  extendRevealed1,
  extendUnrevealed,
  extendUnrevealed1,
  floodLevelL,
  getLeadPlayer,
  getLocationMetaDefault,
  getSetAsideCard,
  globalMetaL,
  investigateSkillL,
  is,
  labelL,
  location,
  locationResignAction,
  locationWith,
  push,
  pushAll,
  revealedConnectedMatchersL,
  setConnectsTo,
  setLabel,
  setMeta,
  shroudL,
  symbolLabel,
  tokensL,
  veiled,
  veiled1,
  withDrawCardUnderneathAction,
  withResignAction,
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern FlipThis,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern PlaceDoom,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Classes.HasGame
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Tracing
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict

whenRevealed :: HasGame m => LocationAttrs -> m () -> m ()
whenRevealed attrs body = when attrs.revealed body

whenUnrevealed :: HasGame m => LocationAttrs -> m () -> m ()
whenUnrevealed attrs body = when attrs.unrevealed body

blockedWhenUnrevealed
  :: (HasGame m, Tracing m, MonadWriter (MonoidalMap Target [Modifier]) m) => LocationAttrs -> m ()
blockedWhenUnrevealed attrs = whenUnrevealed attrs $ modifySelf attrs [Blocked]

blockedWhen
  :: (HasGame m, Tracing m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs -> m Bool -> m ()
blockedWhen attrs body = do
  cond <- body
  when cond $ modifySelf attrs [Blocked]

blockedUnless
  :: (HasGame m, Tracing m, MonadWriter (MonoidalMap Target [Modifier]) m) => LocationAttrs -> m Bool -> m ()
blockedUnless attrs body = blockedWhen attrs (not <$> body)

blockedWhenAny
  :: (Query query, HasGame m, Tracing m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs
  -> query
  -> m ()
blockedWhenAny attrs query = blockedWhen attrs (selectAny query)

hereGets
  :: (HasGame m, Tracing m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs -> [ModifierType] -> m ()
hereGets a mods = modifySelect a (investigatorAt a) mods
