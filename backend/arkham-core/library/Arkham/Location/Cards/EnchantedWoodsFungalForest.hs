module Arkham.Location.Cards.EnchantedWoodsFungalForest
  ( enchantedWoodsFungalForest
  , EnchantedWoodsFungalForest(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EnchantedWoodsFungalForest = EnchantedWoodsFungalForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsFungalForest :: LocationCard EnchantedWoodsFungalForest
enchantedWoodsFungalForest = location EnchantedWoodsFungalForest Cards.enchantedWoodsFungalForest 5 (PerPlayer 1)

instance HasAbilities EnchantedWoodsFungalForest where
  getAbilities (EnchantedWoodsFungalForest attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage EnchantedWoodsFungalForest where
  runMessage msg (EnchantedWoodsFungalForest attrs) =
    EnchantedWoodsFungalForest <$> runMessage msg attrs
