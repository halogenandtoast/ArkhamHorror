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
  setSpawnAt,
  spawnAtL,
  surgeIfUnableToSpawnL,
  tokensL,
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

import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (
  HasQueue,
  evalQueueT,
 )
import Arkham.Matcher (LocationMatcher (EmptyLocation))
import Arkham.Modifier
import Arkham.Queue
import Control.Monad.Trans

doesNotReadyDuringUpkeep :: (ReverseQueue m, Sourceable source) => source -> EnemyAttrs -> m ()
doesNotReadyDuringUpkeep source attrs = roundModifier source attrs DoesNotReadyDuringUpkeep

insteadOfDefeat
  :: (HasQueue Message m, AsId enemy, IdOf enemy ~ EnemyId) => enemy -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfDefeat asEnemy body = whenM (beingDefeated asEnemy) do
  cancelEnemyDefeat asEnemy
  pushAll =<< evalQueueT body

-- See: The Spectral Watcher
insteadOfDefeatWithWindows
  :: (HasQueue Message m, HasGame m, ToId enemy EnemyId)
  => enemy -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfDefeatWithWindows e body = whenM (beingDefeated e) do
  cancelEnemyDefeatWithWindows e
  pushAll =<< evalQueueT body

insteadOfEvading
  :: HasQueue Message m => EnemyAttrs -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfEvading attrs body = whenM (beingEvaded attrs) do
  cancelEvadeEnemy attrs
  pushAll =<< evalQueueT body

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

spawnAtEmptyLocation :: EnemyAttrs -> EnemyAttrs
spawnAtEmptyLocation = spawnAtL ?~ SpawnAt EmptyLocation
