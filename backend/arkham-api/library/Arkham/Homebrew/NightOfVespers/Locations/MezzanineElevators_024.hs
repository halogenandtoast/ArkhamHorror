-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.MezzanineElevators_024 (mezzanineElevators_024) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype MezzanineElevators_024 = MezzanineElevators_024 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mezzanineElevators_024 :: LocationCard MezzanineElevators_024
mezzanineElevators_024 = symbolLabel $ location MezzanineElevators_024 Cards.mezzanineElevators_024 3 (PerPlayer 1)

instance RunMessage MezzanineElevators_024 where
  runMessage msg (MezzanineElevators_024 attrs) = MezzanineElevators_024 <$> runMessage msg attrs
