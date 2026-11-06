-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.HotelLobby_020 (hotelLobby_020) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HotelLobby_020 = HotelLobby_020 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelLobby_020 :: LocationCard HotelLobby_020
hotelLobby_020 = symbolLabel $ location HotelLobby_020 Cards.hotelLobby_020 3 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities HotelLobby_020 where
  getAbilities (HotelLobby_020 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage HotelLobby_020 where
  runMessage msg l@(HotelLobby_020 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hedgeMaze
          , Cards.conservatory
          ]
      pure l
    _ -> HotelLobby_020 <$> liftRunMessage msg attrs
