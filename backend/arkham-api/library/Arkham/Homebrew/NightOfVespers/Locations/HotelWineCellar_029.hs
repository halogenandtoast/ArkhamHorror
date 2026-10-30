-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.HotelWineCellar_029 (hotelWineCellar_029) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype HotelWineCellar_029 = HotelWineCellar_029 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelWineCellar_029 :: LocationCard HotelWineCellar_029
hotelWineCellar_029 = location HotelWineCellar_029 Cards.hotelWineCellar_029 3 (PerPlayer 2)

instance RunMessage HotelWineCellar_029 where
  runMessage msg (HotelWineCellar_029 attrs) = HotelWineCellar_029 <$> runMessage msg attrs
