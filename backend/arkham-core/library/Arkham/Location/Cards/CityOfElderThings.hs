module Arkham.Location.Cards.CityOfElderThings
  ( cityOfElderThings
  , CityOfElderThings(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CityOfElderThings = CityOfElderThings LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfElderThings :: LocationCard CityOfElderThings
cityOfElderThings =
  location CityOfElderThings Cards.cityOfElderThings 3 (PerPlayer 2)

instance HasAbilities CityOfElderThings where
  getAbilities (CityOfElderThings attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CityOfElderThings where
  runMessage msg (CityOfElderThings attrs) =
    CityOfElderThings <$> runMessage msg attrs
