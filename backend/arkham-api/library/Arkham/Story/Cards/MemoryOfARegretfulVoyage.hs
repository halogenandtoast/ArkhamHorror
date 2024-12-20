module Arkham.Story.Cards.MemoryOfARegretfulVoyage (memoryOfARegretfulVoyage) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfARegretfulVoyage = MemoryOfARegretfulVoyage StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfARegretfulVoyage :: StoryCard MemoryOfARegretfulVoyage
memoryOfARegretfulVoyage = story MemoryOfARegretfulVoyage Cards.memoryOfARegretfulVoyage

instance RunMessage MemoryOfARegretfulVoyage where
  runMessage msg s@(MemoryOfARegretfulVoyage attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> MemoryOfARegretfulVoyage <$> liftRunMessage msg attrs
