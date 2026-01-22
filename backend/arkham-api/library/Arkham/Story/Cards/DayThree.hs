module Arkham.Story.Cards.DayThree (dayThree) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DayThree = DayThree StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dayThree :: StoryCard DayThree
dayThree = story DayThree Cards.dayThree

instance RunMessage DayThree where
  runMessage msg s@(DayThree attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> DayThree <$> liftRunMessage msg attrs
