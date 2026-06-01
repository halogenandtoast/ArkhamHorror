module Arkham.Story.Cards.TheAbyss (theAbyss) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheAbyss = TheAbyss StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAbyss :: StoryCard TheAbyss
theAbyss = story TheAbyss Cards.theAbyss

instance RunMessage TheAbyss where
  runMessage msg (TheAbyss attrs) =
    runQueueT $ TheAbyss <$> liftRunMessage msg attrs
