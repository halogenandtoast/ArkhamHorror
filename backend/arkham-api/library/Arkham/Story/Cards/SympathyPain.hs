module Arkham.Story.Cards.SympathyPain (sympathyPain) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SympathyPain = SympathyPain StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sympathyPain :: StoryCard SympathyPain
sympathyPain = story SympathyPain Cards.sympathyPain

instance RunMessage SympathyPain where
  runMessage msg s@(SympathyPain attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> SympathyPain <$> liftRunMessage msg attrs
