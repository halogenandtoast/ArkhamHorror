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
  replaceAllMessagesMatching,
 )
import Arkham.Matcher (LocationMatcher (EmptyLocation))
import Arkham.Modifier
import Arkham.Queue
import Arkham.Window qualified as Window
import Control.Monad.Trans

insteadOfDiscarding
  :: HasQueue Message m => EnemyAttrs -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfDiscarding attrs body = do
  msgs <- evalQueueT body
  let
    isEntityDiscarded w = case w.kind of
      Window.EntityDiscarded _ target -> isTarget attrs target
      _ -> False
  lift $ replaceAllMessagesMatching
    \case
      CheckWindows ws -> any isEntityDiscarded ws
      Do (CheckWindows ws) -> any isEntityDiscarded ws
      Discard _ _ target -> isTarget attrs target
      _ -> False
    \case
      CheckWindows ws ->
        case filter (not . isEntityDiscarded) ws of
          [] -> []
          ws' -> [CheckWindows ws']
      Do (CheckWindows ws) ->
        case filter (not . isEntityDiscarded) ws of
          [] -> []
          ws' -> [Do (CheckWindows ws')]
      Discard {} -> msgs
      _ -> error "Invalid replacement"

doesNotReadyDuringUpkeep :: (ReverseQueue m, Sourceable source) => source -> EnemyAttrs -> m ()
doesNotReadyDuringUpkeep source attrs = roundModifier source attrs DoesNotReadyDuringUpkeep

insteadOfDefeat
  :: HasQueue Message m => EnemyAttrs -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfDefeat attrs body = whenM (beingDefeated attrs) do
  cancelEnemyDefeat attrs
  pushAll =<< evalQueueT body

-- See: The Spectral Watcher
insteadOfDefeatWithWindows
  :: (HasQueue Message m, HasGame m)
  => EnemyAttrs -> QueueT Message (QueueT Message m) () -> QueueT Message m ()
insteadOfDefeatWithWindows attrs body = whenM (beingDefeated attrs) do
  cancelEnemyDefeatWithWindows attrs
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

beingDefeated :: (HasQueue Message m, MonadTrans t) => EnemyAttrs -> t m Bool
beingDefeated attrs = fromQueue $ any isDefeatedMessage
 where
  isDefeatedMessage = \case
    EnemyDefeated eid _ _ _ -> eid == attrs.id
    When (EnemyDefeated eid _ _ _) -> eid == attrs.id
    After (EnemyDefeated eid _ _ _) -> eid == attrs.id
    Do msg -> isDefeatedMessage msg
    _ -> False

spawnAtEmptyLocation :: EnemyAttrs -> EnemyAttrs
spawnAtEmptyLocation = spawnAtL ?~ SpawnAt EmptyLocation
