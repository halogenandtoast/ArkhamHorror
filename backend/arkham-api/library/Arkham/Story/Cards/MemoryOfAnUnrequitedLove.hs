module Arkham.Story.Cards.MemoryOfAnUnrequitedLove (memoryOfAnUnrequitedLove) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAnUnrequitedLove = MemoryOfAnUnrequitedLove StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnUnrequitedLove :: StoryCard MemoryOfAnUnrequitedLove
memoryOfAnUnrequitedLove = story MemoryOfAnUnrequitedLove Cards.memoryOfAnUnrequitedLove

instance RunMessage MemoryOfAnUnrequitedLove where
  runMessage msg s@(MemoryOfAnUnrequitedLove attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> MemoryOfAnUnrequitedLove <$> liftRunMessage msg attrs
