module Arkham.Location.Cards.LeMarais217
  ( leMarais217
  , LeMarais217(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype LeMarais217 = LeMarais217 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leMarais217 :: LocationCard LeMarais217
leMarais217 = location
  LeMarais217
  Cards.leMarais217
  3
  (PerPlayer 1)
  Moon
  [Square, Equals, T, Plus]

instance HasAbilities LeMarais217 where
  getAbilities (LeMarais217 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env LeMarais217 where
  runMessage msg (LeMarais217 attrs) = LeMarais217 <$> runMessage msg attrs
