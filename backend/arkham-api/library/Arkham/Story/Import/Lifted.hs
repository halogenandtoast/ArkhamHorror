module Arkham.Story.Import.Lifted (module Arkham.Story.Import.Lifted, module X) where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Helpers.Message as X (targetLabel)
import Arkham.Message as X (
  Message (..),
  UI (..),
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern PlaceClues,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X hiding (story)
import Arkham.Prelude as X
import Arkham.Source as X
import Arkham.Story.Runner as X (
  Is (..),
  IsStory,
  StoryAttrs,
  StoryCard,
  StoryMode (..),
  getAlreadyResolved,
  metaL,
  push,
  pushAll,
  pushWhen,
  story,
 )

import Arkham.Classes.HasQueue (HasQueue, evalQueueT)
import Arkham.Id
import Arkham.Queue
import Arkham.Story.Runner qualified as Msg
import Control.Monad.Trans.Class

afterStoryResolution
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m))
  => StoryAttrs -> QueueT Message (t m) () -> t m ()
afterStoryResolution attrs body = do
  msgs <- evalQueueT body
  lift $ Msg.afterStoryResolution attrs msgs

removeStory :: (AsId story, IdOf story ~ StoryId, ReverseQueue m) => story -> m ()
removeStory x = push $ RemoveStory $ asId x
