module Arkham.Story.Cards.FortunesDisfavor27 (fortunesDisfavor27) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FortunesDisfavor27 = FortunesDisfavor27 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortunesDisfavor27 :: StoryCard FortunesDisfavor27
fortunesDisfavor27 = story FortunesDisfavor27 Cards.fortunesDisfavor27

instance RunMessage FortunesDisfavor27 where
  runMessage msg s@(FortunesDisfavor27 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> FortunesDisfavor27 <$> liftRunMessage msg attrs
