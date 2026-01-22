module Arkham.Story.Cards.DayTwo (dayTwo) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DayTwo = DayTwo StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayTwo :: StoryCard DayTwo
dayTwo = story DayTwo Cards.dayTwo

instance RunMessage DayTwo where
  runMessage msg s@(DayTwo attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> DayTwo <$> liftRunMessage msg attrs
