-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.GalerieLavandiere_015 (galerieLavandiere_015) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype GalerieLavandiere_015 = GalerieLavandiere_015 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

galerieLavandiere_015 :: LocationCard GalerieLavandiere_015
galerieLavandiere_015 = location GalerieLavandiere_015 Cards.galerieLavandiere_015 3 (PerPlayer 1)

instance RunMessage GalerieLavandiere_015 where
  runMessage msg (GalerieLavandiere_015 attrs) = GalerieLavandiere_015 <$> runMessage msg attrs
