-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.MezzanineElevators_023 (mezzanineElevators_023) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype MezzanineElevators_023 = MezzanineElevators_023 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mezzanineElevators_023 :: LocationCard MezzanineElevators_023
mezzanineElevators_023 = symbolLabel $ location MezzanineElevators_023 Cards.mezzanineElevators_023 3 (PerPlayer 1)

instance RunMessage MezzanineElevators_023 where
  runMessage msg (MezzanineElevators_023 attrs) = MezzanineElevators_023 <$> runMessage msg attrs
