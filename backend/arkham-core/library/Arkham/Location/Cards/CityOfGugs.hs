module Arkham.Location.Cards.CityOfGugs (cityOfGugs, CityOfGugs (..)) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude

newtype CityOfGugs = CityOfGugs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfGugs :: LocationCard CityOfGugs
cityOfGugs = location CityOfGugs Cards.cityOfGugs 2 (PerPlayer 1)

instance HasAbilities CityOfGugs where
  getAbilities (CityOfGugs attrs) = extendRevealed attrs []

instance RunMessage CityOfGugs where
  runMessage msg (CityOfGugs attrs) =
    CityOfGugs <$> runMessage msg attrs
