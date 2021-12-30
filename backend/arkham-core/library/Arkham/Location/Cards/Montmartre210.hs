module Arkham.Location.Cards.Montmartre210
  ( montmartre210
  , Montmartre210(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype Montmartre210 = Montmartre210 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre210 :: LocationCard Montmartre210
montmartre210 = location
  Montmartre210
  Cards.montmartre210
  2
  (PerPlayer 1)
  Square
  [Diamond, Triangle, Equals, Moon]

instance HasAbilities Montmartre210 where
  getAbilities (Montmartre210 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Montmartre210 where
  runMessage msg (Montmartre210 attrs) = Montmartre210 <$> runMessage msg attrs
