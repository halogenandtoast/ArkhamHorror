module Arkham.Story.Cards.FortunesDisfavor26 (fortunesDisfavor26) where

import Arkham.Card
import Arkham.Message.Lifted.Story
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted hiding (resolveStory)

newtype FortunesDisfavor26 = FortunesDisfavor26 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortunesDisfavor26 :: StoryCard FortunesDisfavor26
fortunesDisfavor26 = story FortunesDisfavor26 Cards.fortunesDisfavor26

instance RunMessage FortunesDisfavor26 where
  runMessage msg s@(FortunesDisfavor26 attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure s
    Flip iid _ (isTarget attrs -> True) -> do
      let unlikelyInjuries = lookupCard Cards.unlikelyInjuries attrs.cardId
      replaceCard attrs.cardId unlikelyInjuries
      resolveStory iid unlikelyInjuries
      pure s
    _ -> FortunesDisfavor26 <$> liftRunMessage msg attrs
