module Arkham.Story.Cards.MemoryOfARavagedCountry (memoryOfARavagedCountry) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MemoryOfARavagedCountry = MemoryOfARavagedCountry StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfARavagedCountry :: StoryCard MemoryOfARavagedCountry
memoryOfARavagedCountry = story MemoryOfARavagedCountry Cards.memoryOfARavagedCountry

instance RunMessage MemoryOfARavagedCountry where
  runMessage msg s@(MemoryOfARavagedCountry attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> MemoryOfARavagedCountry <$> liftRunMessage msg attrs
