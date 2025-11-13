module Arkham.Story.Cards.FortunesDisfavor25 (fortunesDisfavor25) where

import Arkham.Card
import Arkham.Message.Lifted.Story
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted hiding (resolveStory)

newtype FortunesDisfavor25 = FortunesDisfavor25 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortunesDisfavor25 :: StoryCard FortunesDisfavor25
fortunesDisfavor25 = story FortunesDisfavor25 Cards.fortunesDisfavor25

instance RunMessage FortunesDisfavor25 where
  runMessage msg s@(FortunesDisfavor25 attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure s
    Flip iid _ (isTarget attrs -> True) -> do
      let lightsOut = lookupCard Cards.lightsOut attrs.cardId
      replaceCard attrs.cardId lightsOut
      resolveStory iid lightsOut
      pure s
    _ -> FortunesDisfavor25 <$> liftRunMessage msg attrs
