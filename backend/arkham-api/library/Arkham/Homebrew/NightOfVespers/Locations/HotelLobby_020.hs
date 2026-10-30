-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.HotelLobby_020 (hotelLobby_020) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype HotelLobby_020 = HotelLobby_020 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hotelLobby_020 :: LocationCard HotelLobby_020
hotelLobby_020 = location HotelLobby_020 Cards.hotelLobby_020 3 (PerPlayer 2)

instance RunMessage HotelLobby_020 where
  runMessage msg (HotelLobby_020 attrs) = HotelLobby_020 <$> runMessage msg attrs
