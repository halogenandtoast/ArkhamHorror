module Arkham.Enemy.Import.Lifted (
  module X,
  module Arkham.Enemy.Import.Lifted,
)
where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Enemy.Runner as X (
  EnemyAttrs (..),
  EnemyCard,
  IsEnemy,
  asSelfLocationL,
  attacksL,
  cardCodeL,
  damageStrategyL,
  defeatedL,
  delayEngagementL,
  enemy,
  enemyClues,
  enemyWith,
  evadeL,
  exhaustedL,
  fightL,
  flippedL,
  getEnemyMetaDefault,
  healthL,
  is,
  placementL,
  preyIsBearer,
  preyIsOnlyBearer,
  preyL,
  push,
  pushAll,
  pushM,
  setExhausted,
  setMeta,
  setNoSpawn,
  setOnlyPrey,
  setPrey,
  setPreyIsBearer,
  setPreyIsOnlyBearer,
  setSpawnAt,
  spawnAtL,
  surgeIfUnableToSpawnL,
  tokensL,
  pattern EvadeCriteria,
  pattern R2,
  pattern R3,
 )
import Arkham.GameValue as X
import Arkham.Helpers.Ability as X
import Arkham.Helpers.Choices as X
import Arkham.Helpers.Modifiers as X (toModifiers)
import Arkham.Id as X
import Arkham.Message as X (
  Message (..),
  StoryMode (..),
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
import Arkham.Source as X
import Arkham.Spawn as X
import Arkham.Target as X

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (
  HasQueue,
 )
import Arkham.DefeatedBy
import Arkham.Effect.Window
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.I18n
import Arkham.Matcher hiding (AssetDefeated, DealtDamage, EnemyAttacks, EnemyEvaded)
import Arkham.Modifier
import Arkham.Name
import Arkham.Queue
import Arkham.Tracing
import Arkham.Window qualified as Window
import Control.Monad.Trans

doesNotReadyDuringUpkeep :: (ReverseQueue m, Sourceable source) => source -> EnemyAttrs -> m ()
doesNotReadyDuringUpkeep source attrs = roundModifier source attrs DoesNotReadyDuringUpkeep

insteadOfDefeat
  :: (HasQueue Message m, AsId enemy, IdOf enemy ~ EnemyId)
  => enemy -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfDefeat asEnemy body = whenM (beingDefeated asEnemy) do
  cancelEnemyDefeat asEnemy
  pushAll =<< capture body

-- See: The Spectral Watcher
insteadOfDefeatWithWindows
  :: (HasQueue Message m, Tracing m, HasGame m, ToId enemy EnemyId, CardGen m)
  => enemy -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfDefeatWithWindows e body = whenBeingDefeated e \miid defeatedBy -> do
  cancelEnemyDefeat e
  pushAll =<< capture body
  checkAfter $ Window.EnemyDefeated miid defeatedBy (asId e)

insteadOfEvading
  :: HasQueue Message m => EnemyAttrs -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfEvading attrs body = whenM (beingEvaded attrs) do
  cancelEvadeEnemy attrs
  pushAll =<< capture body

cancelEvadeEnemy :: (HasQueue Message m, MonadTrans t) => EnemyAttrs -> t m ()
cancelEvadeEnemy attrs = matchingDon't isEvadedMessage
 where
  isEvadedMessage = \case
    EnemyEvaded _ eid -> eid == attrs.id
    Do msg -> isEvadedMessage msg
    _ -> False

beingEvaded :: (HasQueue Message m, MonadTrans t) => EnemyAttrs -> t m Bool
beingEvaded attrs = fromQueue $ any isEvadedMessage
 where
  isEvadedMessage = \case
    EnemyEvaded _ eid -> eid == attrs.id
    Do msg -> isEvadedMessage msg
    _ -> False

beingDefeated :: (HasQueue Message m, MonadTrans t, ToId enemy EnemyId) => enemy -> t m Bool
beingDefeated (asId -> enemyId) = fromQueue $ any isDefeatedMessage
 where
  isDefeatedMessage = \case
    Defeated (EnemyTarget eid) _ _ _ -> eid == enemyId
    When (Defeated (EnemyTarget eid) _ _ _) -> eid == enemyId
    After (Defeated (EnemyTarget eid) _ _ _) -> eid == enemyId
    Do msg -> isDefeatedMessage msg
    _ -> False

whenBeingDefeated
  :: (ToId enemy EnemyId, HasGame m)
  => enemy -> (Maybe InvestigatorId -> DefeatedBy -> m ()) -> m ()
whenBeingDefeated (asId -> enemyId) f = go . concat =<< getWindowStack
 where
  go = \case
    (Window.windowType -> Window.EnemyDefeated miid source eid) : _ | eid == enemyId -> f miid source
    (_ : rest) -> go rest
    [] -> pure ()

spawnAtEmptyLocation :: EnemyAttrs -> EnemyAttrs
spawnAtEmptyLocation = spawnAtL ?~ SpawnAt EmptyLocation

-- Sometimes we want to trigger something after an enemy is defeated, but the
-- IfEnemyDefeated window doesn't work on the enemy itself, to bypass this we
-- let the enemy continue using the Defeated (EnemyTarget window) but queues an effect
-- that triggers the IfEnemyDefeated ability
deathRattle
  :: (ReverseQueue m, ToId a EnemyId, HasCardCode a, Named a, Sourceable a)
  => a -> Int -> Criterion -> m ()
deathRattle attrs n criteria =
  createAbilityEffect (EffectDefeatWindow $ asId attrs)
    $ withI18n
    $ withI18nTooltip (cardNameVar attrs $ ikey "name")
    $ (restricted attrs n criteria $ triggered_ $ IfEnemyDefeated #after You ByAny (be $ asId attrs))
      { abilityDisplayAs = Just DisplayAsCard
      , abilitySource = IndexedSource n (toSource attrs)
      }
