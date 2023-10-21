module Arkham.Location.Cards.Restaurant
  ( restaurant
  , Restaurant(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Restaurant = Restaurant LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restaurant :: LocationCard Restaurant
restaurant = location Restaurant Cards.restaurant 4 (PerPlayer 1)

instance HasAbilities Restaurant where
  getAbilities (Restaurant attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Restaurant where
  runMessage msg (Restaurant attrs) =
    Restaurant <$> runMessage msg attrs
