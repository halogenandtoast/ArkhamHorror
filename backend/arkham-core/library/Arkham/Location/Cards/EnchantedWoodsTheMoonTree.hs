module Arkham.Location.Cards.EnchantedWoodsTheMoonTree
  ( enchantedWoodsTheMoonTree
  , EnchantedWoodsTheMoonTree(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EnchantedWoodsTheMoonTree = EnchantedWoodsTheMoonTree LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsTheMoonTree :: LocationCard EnchantedWoodsTheMoonTree
enchantedWoodsTheMoonTree = location EnchantedWoodsTheMoonTree Cards.enchantedWoodsTheMoonTree 3 (PerPlayer 1)

instance HasAbilities EnchantedWoodsTheMoonTree where
  getAbilities (EnchantedWoodsTheMoonTree attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage EnchantedWoodsTheMoonTree where
  runMessage msg (EnchantedWoodsTheMoonTree attrs) =
    EnchantedWoodsTheMoonTree <$> runMessage msg attrs
