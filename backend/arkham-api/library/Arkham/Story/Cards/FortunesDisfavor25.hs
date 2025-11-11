module Arkham.Story.Cards.FortunesDisfavor25 (fortunesDisfavor25) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FortunesDisfavor25 = FortunesDisfavor25 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortunesDisfavor25 :: StoryCard FortunesDisfavor25
fortunesDisfavor25 = story FortunesDisfavor25 Cards.fortunesDisfavor25

instance RunMessage FortunesDisfavor25 where
  runMessage msg s@(FortunesDisfavor25 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> FortunesDisfavor25 <$> liftRunMessage msg attrs
