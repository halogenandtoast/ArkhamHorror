-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.LuxurySuites_021 (luxurySuites_021) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype LuxurySuites_021 = LuxurySuites_021 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

luxurySuites_021 :: LocationCard LuxurySuites_021
luxurySuites_021 = symbolLabel $ location LuxurySuites_021 Cards.luxurySuites_021 2 (PerPlayer 2)

instance RunMessage LuxurySuites_021 where
  runMessage msg (LuxurySuites_021 attrs) = LuxurySuites_021 <$> runMessage msg attrs
