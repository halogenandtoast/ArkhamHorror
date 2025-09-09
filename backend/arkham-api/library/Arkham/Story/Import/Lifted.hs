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
import Arkham.Message.Story as X
import Arkham.Prelude as X
import Arkham.Source as X
import Arkham.Story.Runner as X (
  Is (..),
  IsStory,
  StoryAttrs,
  StoryCard,
  getAlreadyResolved,
  metaL,
  placementL,
  push,
  pushAll,
  pushWhen,
  removeAfterResolutionL,
  story,
  storyWith,
 )
import Arkham.Target as X

import Arkham.Card
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Id
import Arkham.Queue
import Arkham.Story.Runner qualified as Msg
import Control.Monad.Trans.Class

afterStoryResolution
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m))
  => StoryAttrs -> QueueT Message (t m) () -> t m ()
afterStoryResolution attrs body = do
  msgs <- capture body
  lift $ Msg.afterStoryResolution attrs msgs

removeStory :: (AsId story, IdOf story ~ StoryId, ReverseQueue m) => story -> m ()
removeStory x = push $ RemoveStory $ asId x

resolveStory
  :: (ToId investigator InvestigatorId, ToId story StoryId, ReverseQueue m)
  => investigator -> story -> m ()
resolveStory (asId -> iid) (asId -> storyId) = push $ ResolveStory iid ResolveIt storyId

persistStory
  :: HasAttrs a StoryAttrs
  => CardBuilder (Maybe Target, StoryId) a -> CardBuilder (Maybe Target, StoryId) a
persistStory = fmap (overAttrs (\a -> a {Msg.storyRemoveAfterResolution = False}))

pattern ResolveThisStory :: InvestigatorId -> StoryId -> Message
pattern ResolveThisStory iid sid <- StoryMessage (ResolveStory iid ResolveIt sid)

pattern DoNotResolveThisStory :: InvestigatorId -> StoryId -> Message
pattern DoNotResolveThisStory iid sid <- StoryMessage (ResolveStory iid DoNotResolveIt sid)
