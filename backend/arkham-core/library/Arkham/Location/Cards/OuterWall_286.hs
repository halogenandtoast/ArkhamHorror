module Arkham.Location.Cards.OuterWall_286
  ( outerWall_286
  , OuterWall_286(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype OuterWall_286 = OuterWall_286 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerWall_286 :: LocationCard OuterWall_286
outerWall_286 = location
  OuterWall_286
  Cards.outerWall_286
  4
  (PerPlayer 1)
  Triangle
  [Squiggle, Diamond, Equals]

instance HasAbilities OuterWall_286 where
  getAbilities (OuterWall_286 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage OuterWall_286 where
  runMessage msg (OuterWall_286 attrs) = OuterWall_286 <$> runMessage msg attrs
