module Arkham.Location.Cards.PathOfThorns
  ( pathOfThorns
  , PathOfThorns(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype PathOfThorns = PathOfThorns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathOfThorns :: LocationCard PathOfThorns
pathOfThorns = location
  PathOfThorns
  Cards.pathOfThorns
  3
  (PerPlayer 1)
  Square
  [Circle, Diamond, Triangle, Squiggle]

instance HasAbilities PathOfThorns where
  getAbilities (PathOfThorns attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage PathOfThorns where
  runMessage msg (PathOfThorns attrs) = PathOfThorns <$> runMessage msg attrs
