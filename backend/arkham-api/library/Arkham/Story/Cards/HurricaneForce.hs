module Arkham.Story.Cards.HurricaneForce (hurricaneForce) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HurricaneForce = HurricaneForce StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hurricaneForce :: StoryCard HurricaneForce
hurricaneForce = story HurricaneForce Cards.hurricaneForce

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage HurricaneForce where
  runMessage msg (HurricaneForce attrs) = runQueueT $ HurricaneForce <$> liftRunMessage msg attrs
