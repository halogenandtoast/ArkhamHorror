module Arkham.Location.Cards.EnchantedWoodsMysticalForest (
  enchantedWoodsMysticalForest,
  EnchantedWoodsMysticalForest (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EnchantedWoodsMysticalForest = EnchantedWoodsMysticalForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsMysticalForest :: LocationCard EnchantedWoodsMysticalForest
enchantedWoodsMysticalForest = location EnchantedWoodsMysticalForest Cards.enchantedWoodsMysticalForest 4 (PerPlayer 1)

instance HasAbilities EnchantedWoodsMysticalForest where
  getAbilities (EnchantedWoodsMysticalForest attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage EnchantedWoodsMysticalForest where
  runMessage msg (EnchantedWoodsMysticalForest attrs) =
    EnchantedWoodsMysticalForest <$> runMessage msg attrs
