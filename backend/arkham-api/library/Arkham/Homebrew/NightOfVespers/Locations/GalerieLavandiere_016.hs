-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.GalerieLavandiere_016 (galerieLavandiere_016) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype GalerieLavandiere_016 = GalerieLavandiere_016 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

galerieLavandiere_016 :: LocationCard GalerieLavandiere_016
galerieLavandiere_016 = symbolLabel $ location GalerieLavandiere_016 Cards.galerieLavandiere_016 3 (PerPlayer 1)

instance RunMessage GalerieLavandiere_016 where
  runMessage msg (GalerieLavandiere_016 attrs) = GalerieLavandiere_016 <$> runMessage msg attrs
