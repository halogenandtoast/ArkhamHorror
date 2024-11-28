module Arkham.Story.Cards.MadnessInside
  ( MadnessInside(..)
  , madnessInside
  ) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MadnessInside = MadnessInside StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessInside :: StoryCard MadnessInside
madnessInside = story MadnessInside Cards.madnessInside

instance RunMessage MadnessInside where
  runMessage msg s@(MadnessInside attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> MadnessInside <$> liftRunMessage msg attrs
