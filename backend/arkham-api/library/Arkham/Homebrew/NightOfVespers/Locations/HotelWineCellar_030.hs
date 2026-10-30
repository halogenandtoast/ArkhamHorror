-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.HotelWineCellar_030 (hotelWineCellar_030) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype HotelWineCellar_030 = HotelWineCellar_030 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelWineCellar_030 :: LocationCard HotelWineCellar_030
hotelWineCellar_030 = symbolLabel $ location HotelWineCellar_030 Cards.hotelWineCellar_030 3 (PerPlayer 2)

instance RunMessage HotelWineCellar_030 where
  runMessage msg (HotelWineCellar_030 attrs) = HotelWineCellar_030 <$> runMessage msg attrs
