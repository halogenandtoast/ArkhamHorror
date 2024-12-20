module Arkham.Story.Cards.MemoryOfAnUnspeakableEvil (memoryOfAnUnspeakableEvil) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfAnUnspeakableEvil = MemoryOfAnUnspeakableEvil StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnUnspeakableEvil :: StoryCard MemoryOfAnUnspeakableEvil
memoryOfAnUnspeakableEvil = story MemoryOfAnUnspeakableEvil Cards.memoryOfAnUnspeakableEvil

instance RunMessage MemoryOfAnUnspeakableEvil where
  runMessage msg s@(MemoryOfAnUnspeakableEvil attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> MemoryOfAnUnspeakableEvil <$> liftRunMessage msg attrs
