module Arkham.Location.Cards.CityOfTheUnseen
  ( cityOfTheUnseen
  , CityOfTheUnseen(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CityOfTheUnseen = CityOfTheUnseen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheUnseen :: LocationCard CityOfTheUnseen
cityOfTheUnseen = location CityOfTheUnseen Cards.cityOfTheUnseen 4 (Static 1)

instance HasAbilities CityOfTheUnseen where
  getAbilities (CityOfTheUnseen attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CityOfTheUnseen where
  runMessage msg (CityOfTheUnseen attrs) =
    CityOfTheUnseen <$> runMessage msg attrs
