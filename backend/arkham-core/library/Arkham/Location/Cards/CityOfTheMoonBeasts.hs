module Arkham.Location.Cards.CityOfTheMoonBeasts (
  cityOfTheMoonBeasts,
  CityOfTheMoonBeasts (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CityOfTheMoonBeasts = CityOfTheMoonBeasts LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheMoonBeasts :: LocationCard CityOfTheMoonBeasts
cityOfTheMoonBeasts = location CityOfTheMoonBeasts Cards.cityOfTheMoonBeasts 0 (Static 0)

instance HasAbilities CityOfTheMoonBeasts where
  getAbilities (CityOfTheMoonBeasts attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage CityOfTheMoonBeasts where
  runMessage msg (CityOfTheMoonBeasts attrs) =
    CityOfTheMoonBeasts <$> runMessage msg attrs
