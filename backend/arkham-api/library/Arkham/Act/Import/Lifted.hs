module Arkham.Act.Import.Lifted (
  module X,
  module Arkham.Act.Import.Lifted,
)
where

import Arkham.Act.Helpers as X (groupClueCost)
import Arkham.Act.Runner as X (
  ActAttrs (..),
  ActCard,
  ActSide (..),
  AdvancementMethod (..),
  IsAct,
  Message (..),
  ShuffleIn (..),
  act,
  actWith,
  isSide,
  metaL,
  onSide,
  push,
  pushAll,
  sequenceL,
  targetLabel,
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern R1,
  pattern R2,
  pattern R3,
  pattern R4,
  pattern R5,
  pattern R6,
  pattern R7,
  pattern R8,
  pattern UseThisAbility,
 )
import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Cost as X
import Arkham.GameValue as X
import Arkham.Helpers.Log as X (getHasRecord, whenHasRecord)
import Arkham.Helpers.Query as X (getLead, getLeadPlayer, getSetAsideCard)
import Arkham.Id as X
import Arkham.Message as X (
  -- Pattern synonyms that aren't re-exported by Arkham.Act.Runner because the
  -- corresponding names are hidden from its Helpers.Message import to avoid
  -- matcher clashes. Act cards that use the message form import them here.

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
  pattern EnemyAttacks,
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
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Text as X

import Arkham.Ability.Types
import Arkham.Card.CardDef
import Arkham.Helpers.Act qualified as Msg
import Arkham.Matcher

advanceVia
  :: (ReverseQueue m, EntityId a ~ ActId, Sourceable source, Entity a)
  => AdvancementMethod
  -> a
  -> source
  -> m ()
advanceVia method actId source = push $ Msg.advanceVia method actId source

ifEnemyDefeated :: CardDef -> WindowMatcher
ifEnemyDefeated = ifEnemyDefeatedMatch . enemyIs

ifEnemyDefeatedMatch :: EnemyMatcher -> WindowMatcher
ifEnemyDefeatedMatch = IfEnemyDefeated #after Anyone ByAny

actAbilities
  :: (EntityAttrs act ~ ActAttrs, Entity act) => (ActAttrs -> [Ability]) -> act -> [Ability]
actAbilities = actAbilities' A

actAbilities1
  :: (EntityAttrs act ~ ActAttrs, Entity act) => (ActAttrs -> Ability) -> act -> [Ability]
actAbilities1 = actAbilities1' A

actAbilities'
  :: (EntityAttrs act ~ ActAttrs, Entity act) => ActSide -> (ActAttrs -> [Ability]) -> act -> [Ability]
actAbilities' side abilities (toAttrs -> attrs) = extend attrs $ guard (onSide side attrs) *> abilities attrs

actAbilities1'
  :: (EntityAttrs act ~ ActAttrs, Entity act) => ActSide -> (ActAttrs -> Ability) -> act -> [Ability]
actAbilities1' side ability (toAttrs -> attrs) = extend attrs $ guard (onSide side attrs) *> [ability attrs]

removeAct :: ReverseQueue m => ActAttrs -> m ()
removeAct = toDiscard GameSource

setMeta :: ToJSON a => a -> ActAttrs -> ActAttrs
setMeta v attrs = attrs {actMeta = toJSON v}
