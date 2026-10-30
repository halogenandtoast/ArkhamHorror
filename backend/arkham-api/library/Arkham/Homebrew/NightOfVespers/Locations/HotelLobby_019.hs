-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.HotelLobby_019 (hotelLobby_019) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype HotelLobby_019 = HotelLobby_019 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelLobby_019 :: LocationCard HotelLobby_019
hotelLobby_019 = location HotelLobby_019 Cards.hotelLobby_019 3 (PerPlayer 2)

instance RunMessage HotelLobby_019 where
  runMessage msg (HotelLobby_019 attrs) = HotelLobby_019 <$> runMessage msg attrs
