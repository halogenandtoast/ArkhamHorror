module Arkham.Location.Cards.RiverCanyon
  ( riverCanyon
  , RiverCanyon(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype RiverCanyon = RiverCanyon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverCanyon :: LocationCard RiverCanyon
riverCanyon = location
  RiverCanyon
  Cards.riverCanyon
  4
  (PerPlayer 1)
  Diamond
  [Circle, Moon, Heart, Triangle, Square]

instance HasAbilities RiverCanyon where
  getAbilities (RiverCanyon attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage RiverCanyon where
  runMessage msg (RiverCanyon attrs) = RiverCanyon <$> runMessage msg attrs
