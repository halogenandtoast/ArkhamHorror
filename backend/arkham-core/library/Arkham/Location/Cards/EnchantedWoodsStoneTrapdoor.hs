module Arkham.Location.Cards.EnchantedWoodsStoneTrapdoor
  ( enchantedWoodsStoneTrapdoor
  , EnchantedWoodsStoneTrapdoor(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EnchantedWoodsStoneTrapdoor = EnchantedWoodsStoneTrapdoor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsStoneTrapdoor :: LocationCard EnchantedWoodsStoneTrapdoor
enchantedWoodsStoneTrapdoor = location EnchantedWoodsStoneTrapdoor Cards.enchantedWoodsStoneTrapdoor 2 (PerPlayer 1)

instance HasAbilities EnchantedWoodsStoneTrapdoor where
  getAbilities (EnchantedWoodsStoneTrapdoor attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage EnchantedWoodsStoneTrapdoor where
  runMessage msg (EnchantedWoodsStoneTrapdoor attrs) =
    EnchantedWoodsStoneTrapdoor <$> runMessage msg attrs
