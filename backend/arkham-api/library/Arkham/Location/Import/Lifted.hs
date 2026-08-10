module Arkham.Location.Import.Lifted (module X, module Arkham.Location.Import.Lifted) where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Helpers.Message as X (pattern R1, pattern R2)
import Arkham.Helpers.Modifiers as X (toModifiers)
import Arkham.Id as X
import Arkham.Location.Helpers as X (adjacentLocations, connectsToAdjacent)
import Arkham.Message as X (
  pattern AbilityIsSkillTest,
  pattern AfterEnemyAttack,
  pattern AfterEvadeEnemy,
  pattern AfterRevealChaosTokens,
  pattern AfterSkillTestEnds,
  pattern AfterSkillTestOption,
  pattern AfterSkillTestQuiet,
  pattern AfterThisTestResolves,
  pattern AssetDefeated,
  pattern AttackEnemy,
  pattern BeforeCardCost,
  pattern BeforePlayEvent,
  pattern BeforeRevealChaosTokens,
  pattern BeforeSkillTest,
  pattern BeginSkillTestAfterFast,
  pattern BeginSkillTestWithPreMessages,
  pattern BeginSkillTestWithPreMessages',
  pattern CancelAssetHorror,
  pattern CancelHorror,
  pattern CancelSearch,
  pattern ChangeEnemyAttackDetails,
  pattern ChangeEnemyAttackTarget,
  pattern ChangeSkillTestType,
  pattern ChaosTokenCanceled,
  pattern ChaosTokenIgnored,
  pattern ChaosTokenSelected,
  pattern CheckDefeated,
  pattern CheckEnemyEngagement,
  pattern ChooseChaosTokenGroups,
  pattern ChooseEngageEnemy,
  pattern ChooseEvadeEnemy,
  pattern ChooseFightEnemy,
  pattern ChosenEvadeEnemy,
  pattern ClearFound,
  pattern ClearTokens,
  pattern CollectSkillTestOptions,
  pattern CommitCard,
  pattern CommitToSkillTest,
  pattern CreatePendingEvent,
  pattern Damaged,
  pattern DealDamage,
  pattern DefeatEnemy,
  pattern Defeated,
  pattern Devour,
  pattern Devoured,
  pattern DisengageEnemy,
  pattern DisengageEnemyFromAll,
  pattern DrawChaosToken,
  pattern EndSkillTestWindow,
  pattern EnemiesAttack,
  pattern EnemyAttack,
  pattern EnemyAttackFromDiscard,
  pattern EnemyAttackIfEngaged,
  pattern EnemyCheckEngagement,
  pattern EnemyEngageInvestigator,
  pattern EnemyEntered,
  pattern EnemyLocationDefeated,
  pattern EnemyMove,
  pattern EnemySpawn,
  pattern EnemySpawnAtLocationMatching,
  pattern EnemySpawnEngagedWith,
  pattern EnemySpawnEngagedWithPrey,
  pattern EnemySpawnFromOutOfPlay,
  pattern EnemySpawned,
  pattern EnemyWillAttack,
  pattern EngageEnemy,
  pattern EvadeEnemy,
  pattern ExcessHealHorror,
  pattern Exhaust,
  pattern FailSkillTest,
  pattern Failed,
  pattern FailedAttackEnemy,
  pattern FailedSkillTest,
  pattern FightEnemy,
  pattern FinalizeRequestedChaosTokens,
  pattern FinishedEvent,
  pattern FinishedSearch,
  pattern FlipClues,
  pattern FlipDoom,
  pattern ForceChaosTokenDraw,
  pattern ForceChaosTokenDrawToken,
  pattern FoundCards,
  pattern HandleElusive,
  pattern HandleKilledOrInsaneInvestigators,
  pattern HealAllDamage,
  pattern HealAllHorror,
  pattern HealDamage,
  pattern HealHorror,
  pattern HunterMove,
  pattern HuntersMove,
  pattern IncreaseSkillTestDifficulty,
  pattern InitiateEnemyAttack,
  pattern InvestigatorAdjustAssetSlots,
  pattern InvestigatorAdjustSlot,
  pattern InvestigatorAssignDamage,
  pattern InvestigatorClearUnusedAssetSlots,
  pattern InvestigatorCommittedCard,
  pattern InvestigatorCommittedSkill,
  pattern InvestigatorDamage,
  pattern InvestigatorDamageEnemy,
  pattern InvestigatorDamageInvestigator,
  pattern InvestigatorDefeated,
  pattern InvestigatorDirectDamage,
  pattern InvestigatorDiscardAllClues,
  pattern InvestigatorDoAssignDamage,
  pattern InvestigatorDrawEnemy,
  pattern InvestigatorDrewEncounterCard,
  pattern InvestigatorDrewEncounterCardFrom,
  pattern InvestigatorDrewPlayerCardFrom,
  pattern InvestigatorEliminated,
  pattern InvestigatorIsDefeated,
  pattern InvestigatorKilled,
  pattern InvestigatorMulligan,
  pattern InvestigatorPlaceAllCluesOnLocation,
  pattern InvestigatorPlaceCluesOnLocation,
  pattern InvestigatorPlayAsset,
  pattern InvestigatorPlayEvent,
  pattern InvestigatorPlayedAsset,
  pattern InvestigatorResigned,
  pattern InvestigatorSpecific,
  pattern InvestigatorSpendClues,
  pattern InvestigatorWhenDefeated,
  pattern InvestigatorWhenEliminated,
  pattern InvestigatorsMulligan,
  pattern MoveAllCluesTo,
  pattern MoveTokens,
  pattern MoveTokensNoDefeated,
  pattern MoveToward,
  pattern MoveUntil,
  pattern NextChaosBagStep,
  pattern NextSkillTest,
  pattern ObtainChaosToken,
  pattern PassSkillTest,
  pattern PassSkillTestBy,
  pattern PassedSkillTest,
  pattern PatrolMove,
  pattern PerformEnemyAttack,
  pattern PlaceAdditionalDamage,
  pattern PlaceCluesUpToClueValue,
  pattern PlaceTokens,
  pattern PreSearchFound,
  pattern Ready,
  pattern ReadyAlternative,
  pattern ReadyExhausted,
  pattern RecalculateSkillTestResults,
  pattern RecalculateSkillTestResultsCanChangeAutomatic,
  pattern RemoveAllChaosTokens,
  pattern RemoveAllClues,
  pattern RemoveAllDoom,
  pattern RemoveAllDoomFromPlay,
  pattern RemoveAllTokens,
  pattern RemoveAsset,
  pattern RemoveChaosToken,
  pattern RemoveEnemy,
  pattern RemoveEvent,
  pattern RemoveLocation,
  pattern RemoveSkill,
  pattern RemoveTokens,
  pattern RemoveTreachery,
  pattern RepeatSkillTest,
  pattern ReplaceCurrentDraw,
  pattern ReplaceEntireDraw,
  pattern ReplaceSkillTestSkill,
  pattern RequestChaosTokens,
  pattern RequestedChaosTokens,
  pattern RerunSkillTest,
  pattern ResetChaosTokens,
  pattern ResetTokenPool,
  pattern Resign,
  pattern ResignWith,
  pattern ResolveChaosToken,
  pattern ResolveSearch,
  pattern ReturnChaosTokens,
  pattern ReturnChaosTokensToPool,
  pattern ReturnSkillTestRevealedChaosTokens,
  pattern RevealChaosToken,
  pattern RevealSkillTestChaosTokens,
  pattern RevealSkillTestChaosTokensAgain,
  pattern RevelationSkillTest,
  pattern RunBag,
  pattern RunDrawFromBag,
  pattern RunSkillTest,
  pattern SealChaosToken,
  pattern SealedChaosToken,
  pattern Search,
  pattern SearchCollectionForRandom,
  pattern SearchEnded,
  pattern SearchFound,
  pattern SearchNoneFound,
  pattern SetChaosBagChoice,
  pattern SetChaosTokenAside,
  pattern SetChaosTokens,
  pattern SetChaosTokensForScenario,
  pattern SetSkillTestResolveFailureInvestigator,
  pattern SetSkillTestTarget,
  pattern SilentRevealChaosToken,
  pattern SkillTestApplyResults,
  pattern SkillTestApplyResultsAfter,
  pattern SkillTestAsk,
  pattern SkillTestCommitCard,
  pattern SkillTestEnded,
  pattern SkillTestEnds,
  pattern SkillTestResultOption,
  pattern SkillTestResultOptions,
  pattern SkillTestResults,
  pattern SkillTestUncommitCard,
  pattern StartSkillTest,
  pattern Successful,
  pattern SwapChaosToken,
  pattern TargetResolveChaosToken,
  pattern TriggerSkillTest,
  pattern TryEvadeEnemy,
  pattern UnsealChaosToken,
  pattern UpdateSearchReturnStrategy,
  pattern WillMoveEnemy,
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
  setCostToEnterUnrevealed,
  setLabel,
  setMeta,
  shroudL,
  symbolLabel,
  tokensL,
  veiled,
  veiled1,
  withDrawCardUnderneathAction,
  withResignAction,
  withXShroud,
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
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict

whenRevealed :: HasGame m => LocationAttrs -> m () -> m ()
whenRevealed attrs body = when attrs.revealed body

whenUnrevealed :: HasGame m => LocationAttrs -> m () -> m ()
whenUnrevealed attrs body = when attrs.unrevealed body

blockedWhenUnrevealed
  :: (HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m) => LocationAttrs -> m ()
blockedWhenUnrevealed attrs = whenUnrevealed attrs $ modifySelf attrs [Blocked]

blockedWhen
  :: (HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs -> m Bool -> m ()
blockedWhen attrs body = do
  cond <- body
  when cond $ modifySelf attrs [Blocked]

blockedUnless
  :: (HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs -> m Bool -> m ()
blockedUnless attrs body = blockedWhen attrs (not <$> body)

blockedWhenAny
  :: (Query query, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs
  -> query
  -> m ()
blockedWhenAny attrs query = blockedWhen attrs (selectAny query)

hereGets
  :: (HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs -> [ModifierType] -> m ()
hereGets a mods = modifySelect a (investigatorAt a) mods
