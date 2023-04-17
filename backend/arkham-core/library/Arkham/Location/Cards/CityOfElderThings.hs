module Arkham.Location.Cards.CityOfElderThings
  ( cityOfElderThings
  , CityOfElderThings(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message

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
  runMessage msg (CityOfElderThings attrs) = case msg of
    RevealLocation _ lid | lid == toId attrs -> do
      CityOfElderThings <$> runMessage msg (attrs & labelL .~ "cityOfElderThings")
    _ -> CityOfElderThings <$> runMessage msg attrs
