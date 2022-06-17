module Arkham.Location.Cards.OuterWall_285
  ( outerWall_285
  , OuterWall_285(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype OuterWall_285 = OuterWall_285 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerWall_285 :: LocationCard OuterWall_285
outerWall_285 = location
  OuterWall_285
  Cards.outerWall_285
  2
  (PerPlayer 2)
  Triangle
  [Squiggle, Diamond, Equals]

instance HasAbilities OuterWall_285 where
  getAbilities (OuterWall_285 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage OuterWall_285 where
  runMessage msg (OuterWall_285 attrs) = OuterWall_285 <$> runMessage msg attrs
