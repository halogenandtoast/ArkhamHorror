module Arkham.Location.Cards.NorthTower_287
  ( northTower_287
  , NorthTower_287(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype NorthTower_287 = NorthTower_287 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northTower_287 :: LocationCard NorthTower_287
northTower_287 = location NorthTower_287 Cards.northTower_287 2 (PerPlayer 2) Diamond [Squiggle, Triangle, Equals]

instance HasAbilities NorthTower_287 where
  getAbilities (NorthTower_287 attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage NorthTower_287 where
  runMessage msg (NorthTower_287 attrs) =
    NorthTower_287 <$> runMessage msg attrs
