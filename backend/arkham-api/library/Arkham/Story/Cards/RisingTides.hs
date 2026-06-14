module Arkham.Story.Cards.RisingTides (risingTides) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RisingTides = RisingTides StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

risingTides :: StoryCard RisingTides
risingTides = story RisingTides Cards.risingTides

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage RisingTides where
  runMessage msg (RisingTides attrs) = runQueueT $ RisingTides <$> liftRunMessage msg attrs
