module Arkham.Location.Cards.EnchantedWoodsGreatStoneCircle
  ( enchantedWoodsGreatStoneCircle
  , EnchantedWoodsGreatStoneCircle(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EnchantedWoodsGreatStoneCircle = EnchantedWoodsGreatStoneCircle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsGreatStoneCircle :: LocationCard EnchantedWoodsGreatStoneCircle
enchantedWoodsGreatStoneCircle = location EnchantedWoodsGreatStoneCircle Cards.enchantedWoodsGreatStoneCircle 1 (PerPlayer 1)

instance HasAbilities EnchantedWoodsGreatStoneCircle where
  getAbilities (EnchantedWoodsGreatStoneCircle attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage EnchantedWoodsGreatStoneCircle where
  runMessage msg (EnchantedWoodsGreatStoneCircle attrs) =
    EnchantedWoodsGreatStoneCircle <$> runMessage msg attrs
