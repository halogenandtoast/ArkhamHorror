module Arkham.Location.Cards.GrandRue
  ( grandRue
  , GrandRue(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype GrandRue = GrandRue LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandRue :: LocationCard GrandRue
grandRue = location
  GrandRue
  Cards.grandRue
  1
  (PerPlayer 1)
  Squiggle
  [Circle, Triangle, Diamond, Equals]

instance HasAbilities GrandRue where
  getAbilities (GrandRue attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage GrandRue where
  runMessage msg (GrandRue attrs) = GrandRue <$> runMessage msg attrs
