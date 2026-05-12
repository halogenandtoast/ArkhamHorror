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
  pattern UseThisAbility,
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
