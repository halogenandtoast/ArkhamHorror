module Arkham.Story.Cards.AlaskanWilds (alaskanWilds) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AlaskanWilds = AlaskanWilds StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alaskanWilds :: StoryCard AlaskanWilds
alaskanWilds = story AlaskanWilds Cards.alaskanWilds

instance RunMessage AlaskanWilds where
  runMessage msg s@(AlaskanWilds attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> AlaskanWilds <$> liftRunMessage msg attrs
