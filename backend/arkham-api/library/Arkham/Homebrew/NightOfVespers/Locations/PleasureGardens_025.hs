-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.PleasureGardens_025 (pleasureGardens_025) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype PleasureGardens_025 = PleasureGardens_025 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pleasureGardens_025 :: LocationCard PleasureGardens_025
pleasureGardens_025 = location PleasureGardens_025 Cards.pleasureGardens_025 3 (PerPlayer 2)

instance RunMessage PleasureGardens_025 where
  runMessage msg (PleasureGardens_025 attrs) = PleasureGardens_025 <$> runMessage msg attrs
