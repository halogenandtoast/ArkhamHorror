module Arkham.Story.Cards.MemoryOfAMissingFather (memoryOfAMissingFather) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAMissingFather = MemoryOfAMissingFather StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAMissingFather :: StoryCard MemoryOfAMissingFather
memoryOfAMissingFather = story MemoryOfAMissingFather Cards.memoryOfAMissingFather

instance RunMessage MemoryOfAMissingFather where
  runMessage msg s@(MemoryOfAMissingFather attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> MemoryOfAMissingFather <$> liftRunMessage msg attrs
