module Arkham.Story.Cards.BarriersDecoysAndTraps (barriersDecoysAndTraps) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype BarriersDecoysAndTraps = BarriersDecoysAndTraps StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barriersDecoysAndTraps :: StoryCard BarriersDecoysAndTraps
barriersDecoysAndTraps = story BarriersDecoysAndTraps Cards.barriersDecoysAndTraps

instance RunMessage BarriersDecoysAndTraps where
  runMessage msg s@(BarriersDecoysAndTraps attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> BarriersDecoysAndTraps <$> liftRunMessage msg attrs
