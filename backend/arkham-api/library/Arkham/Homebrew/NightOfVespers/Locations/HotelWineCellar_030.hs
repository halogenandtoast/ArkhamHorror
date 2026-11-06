-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.HotelWineCellar_030 (hotelWineCellar_030) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HotelWineCellar_030 = HotelWineCellar_030 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelWineCellar_030 :: LocationCard HotelWineCellar_030
hotelWineCellar_030 = symbolLabel $ location HotelWineCellar_030 Cards.hotelWineCellar_030 3 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities HotelWineCellar_030 where
  getAbilities (HotelWineCellar_030 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage HotelWineCellar_030 where
  runMessage msg l@(HotelWineCellar_030 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelLobby_019
          , Cards.hotelLobby_020
          , Cards.pleasureGardens_025
          , Cards.pleasureGardens_026
          ]
      pure l
    _ -> HotelWineCellar_030 <$> liftRunMessage msg attrs
