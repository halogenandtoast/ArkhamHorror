module Arkham.Location.Cards.EnchantedWoodsVillageOfZoogs
  ( enchantedWoodsVillageOfZoogs
  , EnchantedWoodsVillageOfZoogs(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EnchantedWoodsVillageOfZoogs = EnchantedWoodsVillageOfZoogs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsVillageOfZoogs :: LocationCard EnchantedWoodsVillageOfZoogs
enchantedWoodsVillageOfZoogs = location EnchantedWoodsVillageOfZoogs Cards.enchantedWoodsVillageOfZoogs 3 (PerPlayer 1)

instance HasAbilities EnchantedWoodsVillageOfZoogs where
  getAbilities (EnchantedWoodsVillageOfZoogs attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage EnchantedWoodsVillageOfZoogs where
  runMessage msg (EnchantedWoodsVillageOfZoogs attrs) =
    EnchantedWoodsVillageOfZoogs <$> runMessage msg attrs
