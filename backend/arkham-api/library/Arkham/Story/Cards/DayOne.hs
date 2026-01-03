module Arkham.Story.Cards.DayOne (dayOne) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DayOne = DayOne StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayOne :: StoryCard DayOne
dayOne = story DayOne Cards.dayOne

instance RunMessage DayOne where
  runMessage msg s@(DayOne attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> DayOne <$> liftRunMessage msg attrs
