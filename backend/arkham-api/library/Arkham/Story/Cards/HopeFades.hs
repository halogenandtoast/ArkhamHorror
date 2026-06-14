module Arkham.Story.Cards.HopeFades (hopeFades) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HopeFades = HopeFades StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hopeFades :: StoryCard HopeFades
hopeFades = story HopeFades Cards.hopeFades

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage HopeFades where
  runMessage msg (HopeFades attrs) = runQueueT $ HopeFades <$> liftRunMessage msg attrs
