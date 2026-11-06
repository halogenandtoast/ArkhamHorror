-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.HotelLobby_019 (hotelLobby_019) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HotelLobby_019 = HotelLobby_019 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelLobby_019 :: LocationCard HotelLobby_019
hotelLobby_019 = symbolLabel $ location HotelLobby_019 Cards.hotelLobby_019 3 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities HotelLobby_019 where
  getAbilities (HotelLobby_019 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage HotelLobby_019 where
  runMessage msg l@(HotelLobby_019 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hedgeMaze
          , Cards.conservatory
          ]
      pure l
    _ -> HotelLobby_019 <$> liftRunMessage msg attrs
