module Arkham.Location.Cards.BleakPlainsBleakDesolation
  ( bleakPlainsBleakDesolation
  , BleakPlainsBleakDesolation(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BleakPlainsBleakDesolation = BleakPlainsBleakDesolation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleakPlainsBleakDesolation :: LocationCard BleakPlainsBleakDesolation
bleakPlainsBleakDesolation = location
  BleakPlainsBleakDesolation
  Cards.bleakPlainsBleakDesolation
  4
  (PerPlayer 1)
  Square
  [Circle, Triangle, Diamond]

instance HasAbilities BleakPlainsBleakDesolation where
  getAbilities (BleakPlainsBleakDesolation attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BleakPlainsBleakDesolation where
  runMessage msg (BleakPlainsBleakDesolation attrs) =
    BleakPlainsBleakDesolation <$> runMessage msg attrs
