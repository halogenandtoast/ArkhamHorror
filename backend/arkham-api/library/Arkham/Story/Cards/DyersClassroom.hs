module Arkham.Story.Cards.DyersClassroom (dyersClassroom) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DyersClassroom = DyersClassroom StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dyersClassroom :: StoryCard DyersClassroom
dyersClassroom = story DyersClassroom Cards.dyersClassroom

instance RunMessage DyersClassroom where
  runMessage msg s@(DyersClassroom attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> DyersClassroom <$> liftRunMessage msg attrs
