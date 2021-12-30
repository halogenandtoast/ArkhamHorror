module Arkham.Location.Cards.Montmartre209
  ( montmartre209
  , Montmartre209(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype Montmartre209 = Montmartre209 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre209 :: LocationCard Montmartre209
montmartre209 = location
  Montmartre209
  Cards.montmartre209
  3
  (PerPlayer 1)
  Square
  [Diamond, Triangle, Equals, Moon]

instance HasAbilities Montmartre209 where
  getAbilities (Montmartre209 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Montmartre209 where
  runMessage msg (Montmartre209 attrs) = Montmartre209 <$> runMessage msg attrs
