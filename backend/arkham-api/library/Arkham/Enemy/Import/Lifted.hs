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
  preyL,
  push,
  pushAll,
  pushM,
  setExhausted,
  setMeta,
  setPrey,
  setPreyIsBearer,
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

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (
  HasQueue,
 )
import Arkham.DefeatedBy
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher (LocationMatcher (EmptyLocation))
import Arkham.Modifier
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
    EnemyDefeated eid _ _ _ -> eid == enemyId
    When (EnemyDefeated eid _ _ _) -> eid == enemyId
    After (EnemyDefeated eid _ _ _) -> eid == enemyId
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
