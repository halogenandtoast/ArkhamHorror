module Arkham.Location.Cards.RopeBridge
  ( ropeBridge
  , RopeBridge(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype RopeBridge = RopeBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ropeBridge :: LocationCard RopeBridge
ropeBridge = location
  RopeBridge
  Cards.ropeBridge
  2
  (PerPlayer 1)
  Moon
  [Circle, Diamond, Heart, T]

instance HasAbilities RopeBridge where
  getAbilities (RopeBridge attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage RopeBridge where
  runMessage msg (RopeBridge attrs) = RopeBridge <$> runMessage msg attrs
