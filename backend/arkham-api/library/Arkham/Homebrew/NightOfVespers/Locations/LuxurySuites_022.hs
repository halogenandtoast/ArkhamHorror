-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.LuxurySuites_022 (luxurySuites_022) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype LuxurySuites_022 = LuxurySuites_022 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

luxurySuites_022 :: LocationCard LuxurySuites_022
luxurySuites_022 = location LuxurySuites_022 Cards.luxurySuites_022 2 (PerPlayer 2)

instance RunMessage LuxurySuites_022 where
  runMessage msg (LuxurySuites_022 attrs) = LuxurySuites_022 <$> runMessage msg attrs
