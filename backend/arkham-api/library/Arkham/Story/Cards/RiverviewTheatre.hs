module Arkham.Story.Cards.RiverviewTheatre (riverviewTheatre) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RiverviewTheatre = RiverviewTheatre StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverviewTheatre :: StoryCard RiverviewTheatre
riverviewTheatre = story RiverviewTheatre Cards.riverviewTheatre

instance RunMessage RiverviewTheatre where
  runMessage msg s@(RiverviewTheatre attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> RiverviewTheatre <$> liftRunMessage msg attrs
