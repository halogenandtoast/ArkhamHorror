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
  pattern AttackEnemy,
  pattern ChooseFightEnemy,
  pattern FailedAttackEnemy,
  pattern FightEnemy,
  pattern AfterEvadeEnemy,
  pattern ChooseEvadeEnemy,
  pattern ChosenEvadeEnemy,
  pattern EnemyEvaded,
  pattern EvadeEnemy,
  pattern TryEvadeEnemy,
  pattern AfterEnemyAttack,
  pattern ChangeEnemyAttackDetails,
  pattern ChangeEnemyAttackTarget,
  pattern EnemiesAttack,
  pattern EnemyAttack,
  pattern EnemyAttacks,
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
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
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
import Arkham.Matcher (TreacheryMatcher(..), treacheryInThreatAreaOf)
import Arkham.Message.Lifted.Placement
import Arkham.Name
import Arkham.SkillType
import Arkham.Tracing
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

guardInThreatArea :: Monad m => InvestigatorId -> TreacheryAttrs -> MaybeT m ()
guardInThreatArea iid attrs = guard $ treacheryInThreatArea iid attrs

isFirstCopyThisPhase :: (HasGame m, Tracing m, HasCardCode a) => a -> m Bool
isFirstCopyThisPhase attrs = do
  drawn <- getAllHistoryField #phase HistoryTreacheriesDrawn
  pure $ count (== toCardCode attrs) drawn == 1

addHiddenToHand :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> m ()
addHiddenToHand iid a = place a (HiddenInHand iid)
