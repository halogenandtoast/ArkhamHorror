module Arkham.Location.Cards.CityOfTheSerpents
  ( cityOfTheSerpents
  , CityOfTheSerpents(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CityOfTheSerpents = CityOfTheSerpents LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheSerpents :: LocationCard CityOfTheSerpents
cityOfTheSerpents = symbolLabel
  $ location CityOfTheSerpents Cards.cityOfTheSerpents 3 (PerPlayer 1)

instance HasAbilities CityOfTheSerpents where
  getAbilities (CityOfTheSerpents attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CityOfTheSerpents where
  runMessage msg (CityOfTheSerpents attrs) =
    CityOfTheSerpents <$> runMessage msg attrs
