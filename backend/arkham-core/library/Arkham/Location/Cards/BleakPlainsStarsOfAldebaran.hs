module Arkham.Location.Cards.BleakPlainsStarsOfAldebaran
  ( bleakPlainsStarsOfAldebaran
  , BleakPlainsStarsOfAldebaran(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import qualified Arkham.Location.Cards as Cards
import Arkham.Location.Runner

newtype BleakPlainsStarsOfAldebaran = BleakPlainsStarsOfAldebaran LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleakPlainsStarsOfAldebaran :: LocationCard BleakPlainsStarsOfAldebaran
bleakPlainsStarsOfAldebaran = location
  BleakPlainsStarsOfAldebaran
  Cards.bleakPlainsStarsOfAldebaran
  4
  (PerPlayer 1)
  Square
  [Circle, Triangle, Diamond]

instance HasAbilities BleakPlainsStarsOfAldebaran where
  getAbilities (BleakPlainsStarsOfAldebaran attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BleakPlainsStarsOfAldebaran where
  runMessage msg (BleakPlainsStarsOfAldebaran attrs) =
    BleakPlainsStarsOfAldebaran <$> runMessage msg attrs
