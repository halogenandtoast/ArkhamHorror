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
  pattern AfterEnemyAttack,
  pattern AfterEvadeEnemy,
  pattern AssetDefeated,
  pattern AttackEnemy,
  pattern ChangeEnemyAttackDetails,
  pattern ChangeEnemyAttackTarget,
  pattern CheckDefeated,
  pattern ChooseEvadeEnemy,
  pattern ChooseFightEnemy,
  pattern ChosenEvadeEnemy,
  pattern DefeatEnemy,
  pattern EnemiesAttack,
  pattern EnemyAttack,
  pattern EnemyAttackFromDiscard,
  pattern EnemyAttackIfEngaged,
  pattern EnemyAttacks,
  pattern EnemyLocationDefeated,
  pattern EnemyWillAttack,
  pattern EvadeEnemy,
  pattern Exhaust,
  pattern FailedAttackEnemy,
  pattern FightEnemy,
  pattern HealAllDamage,
  pattern HealDamage,
  pattern DealDamage,
  pattern Damaged,
  pattern Defeated,
  pattern InitiateEnemyAttack,
  pattern PerformEnemyAttack,
  pattern PlaceAdditionalDamage,
  pattern Ready,
  pattern ReadyAlternative,
  pattern ReadyExhausted,
  pattern TryEvadeEnemy,
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
