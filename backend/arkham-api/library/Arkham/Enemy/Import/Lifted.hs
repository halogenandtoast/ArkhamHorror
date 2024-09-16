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
  defeatedL,
  enemy,
  enemyWith,
  exhaustedL,
  flippedL,
  is,
  placementL,
  preyL,
  push,
  setMeta,
  spawnAtL,
  tokensL,
 )
import Arkham.GameValue as X
import Arkham.Helpers.Ability as X
import Arkham.Helpers.Modifiers as X (toModifiers)
import Arkham.Message as X (
  Message (..),
  StoryMode (..),
  pattern FailedThisSkillTest,
  pattern PassedThisSkillTest,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.Source as X
import Arkham.Spawn as X
import Arkham.Target as X

import Arkham.Classes.HasQueue (HasQueue, evalQueueT, replaceAllMessagesMatching)
import Arkham.Modifier
import Arkham.Queue
import Arkham.Window qualified as Window

disengageEnemyFromAll :: ReverseQueue m => EnemyAttrs -> m ()
disengageEnemyFromAll attrs = push $ DisengageEnemyFromAll attrs.id

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
      Discard _ _ target -> isTarget attrs target
      _ -> False
    \case
      CheckWindows ws ->
        case filter (not . isEntityDiscarded) ws of
          [] -> []
          ws' -> [CheckWindows ws']
      Discard {} -> msgs
      _ -> error "Invalid replacement"

doesNotReadyDuringUpkeep :: (ReverseQueue m, Sourceable source) => source -> EnemyAttrs -> m ()
doesNotReadyDuringUpkeep source attrs = roundModifier source attrs DoesNotReadyDuringUpkeep
