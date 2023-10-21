module Arkham.Location.Cards.HotelRoof
  ( hotelRoof
  , HotelRoof(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HotelRoof = HotelRoof LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelRoof :: LocationCard HotelRoof
hotelRoof = location HotelRoof Cards.hotelRoof 3 (PerPlayer 1)

instance HasAbilities HotelRoof where
  getAbilities (HotelRoof attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HotelRoof where
  runMessage msg (HotelRoof attrs) =
    HotelRoof <$> runMessage msg attrs
