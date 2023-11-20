module Arkham.Location.Cards.EnchantedWoodsGreatStoneCircle (
  enchantedWoodsGreatStoneCircle,
  EnchantedWoodsGreatStoneCircle (..),
)
where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype EnchantedWoodsGreatStoneCircle = EnchantedWoodsGreatStoneCircle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsGreatStoneCircle :: LocationCard EnchantedWoodsGreatStoneCircle
enchantedWoodsGreatStoneCircle = location EnchantedWoodsGreatStoneCircle Cards.enchantedWoodsGreatStoneCircle 1 (PerPlayer 1)

instance HasAbilities EnchantedWoodsGreatStoneCircle where
  getAbilities (EnchantedWoodsGreatStoneCircle attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ RevealLocation #after Anyone $ LocationWithId $ toId attrs]

instance RunMessage EnchantedWoodsGreatStoneCircle where
  runMessage msg l@(EnchantedWoodsGreatStoneCircle attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      laboringGug <- getSetAsideCard Enemies.laboringGug
      pushM $ createEnemyAt_ laboringGug (toId attrs) Nothing
      pure l
    _ -> EnchantedWoodsGreatStoneCircle <$> runMessage msg attrs
