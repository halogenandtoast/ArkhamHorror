module Arkham.Treachery.Import.Lifted (
  module X,
  module Arkham.Treachery.Import.Lifted,
)
where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Helpers.Choices as X
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.Message as X (
  Message (..),
  ShuffleIn (..),
  toMessage,
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
  pattern EnemyEvaded,
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
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
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
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern PatrolMove,
  pattern PerformEnemyAttack,
  pattern PlaceAdditionalDamage,
  pattern PlaceCluesUpToClueValue,
  pattern PlaceDoom,
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
  pattern UseThisAbility,
  pattern WillMoveEnemy,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Treachery.Runner as X (
  Field (..),
  IsTreachery,
  TreacheryAttrs,
  TreacheryCard,
  canBeCommittedL,
  forcedOnElimination,
  metaL,
  on,
  push,
  pushAll,
  pushM,
  pushWhen,
  setMeta,
  tokensL,
  treachery,
  treacheryHorror,
  treacheryInHandOf,
  treacheryInThreatArea,
  treacheryOn,
  treacheryOnAgenda,
  treacheryOnEnemy,
  treacheryOnLocation,
  treacheryOnTopOfDeck,
  treacheryWith,
  waitingL,
  withTreacheryInvestigator,
  pattern PlaceResources,
 )

import Arkham.Card.CardCode
import Arkham.Classes.HasGame
import Arkham.Helpers.History
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Matcher (TreacheryMatcher (..), treacheryInThreatAreaOf)
import Arkham.Message.Lifted.Placement
import Arkham.Name
import Arkham.SkillType
import Arkham.Treachery.Helpers qualified as Msg

revelationSkillTest
  :: (Sourceable source, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> SkillType
  -> GameCalculation
  -> m ()
revelationSkillTest sid iid source sType n = push $ Msg.revelationSkillTest sid iid source sType n

attachTreachery
  :: ( ReverseQueue m
     , AsId a
     , IdOf a ~ TreacheryId
     , Targetable target
     , Show target
     , Msg.NotEqual target InvestigatorId
     )
  => a
  -> target
  -> m ()
attachTreachery a target = push $ Msg.attachTreachery a target

placeInThreatArea
  :: (ReverseQueue m, AsId a, IdOf a ~ TreacheryId)
  => a
  -> InvestigatorId
  -> m ()
placeInThreatArea t = push . Msg.placeInThreatArea t

placeInThreatAreaOnlyOne
  :: (ReverseQueue m, AsId a, IdOf a ~ TreacheryId, Named a)
  => a
  -> InvestigatorId
  -> m ()
placeInThreatAreaOnlyOne t iid = do
  alreadyHasOne <- selectAny $ treacheryInThreatAreaOf iid <> TreacheryWithTitle (toTitle t)
  unless alreadyHasOne $ placeInThreatArea t iid

placeTreachery
  :: (ReverseQueue m, AsId a, IdOf a ~ TreacheryId)
  => a
  -> Placement
  -> m ()
placeTreachery t = push . Msg.PlaceTreachery (asId t)

gainSurge :: (ReverseQueue m, Sourceable a, Targetable a) => a -> m ()
gainSurge = push . Msg.gainSurge
isFirstCopyThisPhase :: (HasGame m, HasCardCode a) => a -> m Bool
isFirstCopyThisPhase attrs = do
  drawn <- getAllHistoryField #phase HistoryTreacheriesDrawn
  pure $ count (== toCardCode attrs) drawn == 1

addHiddenToHand :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> m ()
addHiddenToHand iid a = place a (HiddenInHand iid)
