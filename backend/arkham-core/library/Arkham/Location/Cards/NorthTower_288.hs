module Arkham.Location.Cards.NorthTower_288
  ( northTower_288
  , NorthTower_288(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype NorthTower_288 = NorthTower_288 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northTower_288 :: LocationCard NorthTower_288
northTower_288 = location NorthTower_288 Cards.northTower_288 4 (PerPlayer 1) Diamond [Squiggle, Triangle, Equals]

instance HasAbilities NorthTower_288 where
  getAbilities (NorthTower_288 attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage NorthTower_288 where
  runMessage msg (NorthTower_288 attrs) =
    NorthTower_288 <$> runMessage msg attrs
