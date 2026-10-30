-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.GrandBallroom (grandBallroom) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBallroom = GrandBallroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grandBallroom :: LocationCard GrandBallroom
grandBallroom = location GrandBallroom Cards.grandBallroom 4 (PerPlayer 2)

instance RunMessage GrandBallroom where
  runMessage msg (GrandBallroom attrs) = GrandBallroom <$> runMessage msg attrs
