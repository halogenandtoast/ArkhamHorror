-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.HotelRooftop (hotelRooftop) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype HotelRooftop = HotelRooftop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelRooftop :: LocationCard HotelRooftop
hotelRooftop = symbolLabel $ location HotelRooftop Cards.hotelRooftop 4 (PerPlayer 2)

instance RunMessage HotelRooftop where
  runMessage msg (HotelRooftop attrs) = HotelRooftop <$> runMessage msg attrs
