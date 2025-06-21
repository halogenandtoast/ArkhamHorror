module Arkham.Location.Cards.EnchantedWoodsMysticalForest (enchantedWoodsMysticalForest) where

import Arkham.Cost
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EnchantedWoodsMysticalForest = EnchantedWoodsMysticalForest LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

enchantedWoodsMysticalForest :: LocationCard EnchantedWoodsMysticalForest
enchantedWoodsMysticalForest = location EnchantedWoodsMysticalForest Cards.enchantedWoodsMysticalForest 4 (PerPlayer 1)

instance HasModifiersFor EnchantedWoodsMysticalForest where
  getModifiersFor (EnchantedWoodsMysticalForest a) = do
    modifySelfWhen a (a.clues > 0) [AdditionalCostToLeave $ HandDiscardCost a.clues #any]

instance RunMessage EnchantedWoodsMysticalForest where
  runMessage msg (EnchantedWoodsMysticalForest attrs) =
    EnchantedWoodsMysticalForest <$> runMessage msg attrs
