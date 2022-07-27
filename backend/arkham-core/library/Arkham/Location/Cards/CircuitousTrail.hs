module Arkham.Location.Cards.CircuitousTrail
  ( circuitousTrail
  , CircuitousTrail(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CircuitousTrail = CircuitousTrail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circuitousTrail :: LocationCard CircuitousTrail
circuitousTrail = location
  CircuitousTrail
  Cards.circuitousTrail
  1
  (PerPlayer 1)
  Heart
  [Hourglass, Diamond, Moon, T]

instance HasAbilities CircuitousTrail where
  getAbilities (CircuitousTrail attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CircuitousTrail where
  runMessage msg (CircuitousTrail attrs) =
    CircuitousTrail <$> runMessage msg attrs
