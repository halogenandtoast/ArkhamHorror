-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.Conservatory (conservatory) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Conservatory = Conservatory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

conservatory :: LocationCard Conservatory
conservatory = location Conservatory Cards.conservatory 3 (PerPlayer 2)

instance RunMessage Conservatory where
  runMessage msg (Conservatory attrs) = Conservatory <$> runMessage msg attrs
