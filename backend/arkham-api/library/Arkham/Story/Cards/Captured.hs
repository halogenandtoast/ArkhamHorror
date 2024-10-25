module Arkham.Story.Cards.Captured (Captured (..), captured) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Captured = Captured StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captured :: StoryCard Captured
captured = story Captured Cards.captured

instance RunMessage Captured where
  runMessage msg s@(Captured attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> Captured <$> liftRunMessage msg attrs
