module Arkham.Story.Cards.CoastalWaters (coastalWaters) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype CoastalWaters = CoastalWaters StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coastalWaters :: StoryCard CoastalWaters
coastalWaters = story CoastalWaters Cards.coastalWaters

instance RunMessage CoastalWaters where
  runMessage msg s@(CoastalWaters attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> CoastalWaters <$> liftRunMessage msg attrs
