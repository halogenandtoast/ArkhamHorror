module Arkham.Location.Cards.LeMarais218
  ( leMarais218
  , LeMarais218(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype LeMarais218 = LeMarais218 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leMarais218 :: LocationCard LeMarais218
leMarais218 = location
  LeMarais218
  Cards.leMarais218
  1
  (PerPlayer 1)
  Moon
  [Square, Equals, T, Plus]

instance HasAbilities LeMarais218 where
  getAbilities (LeMarais218 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env LeMarais218 where
  runMessage msg (LeMarais218 attrs) = LeMarais218 <$> runMessage msg attrs
