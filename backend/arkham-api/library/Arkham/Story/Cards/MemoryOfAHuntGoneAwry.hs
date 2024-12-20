module Arkham.Story.Cards.MemoryOfAHuntGoneAwry (memoryOfAHuntGoneAwry) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAHuntGoneAwry = MemoryOfAHuntGoneAwry StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAHuntGoneAwry :: StoryCard MemoryOfAHuntGoneAwry
memoryOfAHuntGoneAwry = story MemoryOfAHuntGoneAwry Cards.memoryOfAHuntGoneAwry

instance RunMessage MemoryOfAHuntGoneAwry where
  runMessage msg s@(MemoryOfAHuntGoneAwry attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> MemoryOfAHuntGoneAwry <$> liftRunMessage msg attrs
