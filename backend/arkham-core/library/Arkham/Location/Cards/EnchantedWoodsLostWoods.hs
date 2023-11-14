module Arkham.Location.Cards.EnchantedWoodsLostWoods
  ( enchantedWoodsLostWoods
  , EnchantedWoodsLostWoods(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EnchantedWoodsLostWoods = EnchantedWoodsLostWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsLostWoods :: LocationCard EnchantedWoodsLostWoods
enchantedWoodsLostWoods = location EnchantedWoodsLostWoods Cards.enchantedWoodsLostWoods 4 (PerPlayer 1)

instance HasAbilities EnchantedWoodsLostWoods where
  getAbilities (EnchantedWoodsLostWoods attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage EnchantedWoodsLostWoods where
  runMessage msg (EnchantedWoodsLostWoods attrs) =
    EnchantedWoodsLostWoods <$> runMessage msg attrs
