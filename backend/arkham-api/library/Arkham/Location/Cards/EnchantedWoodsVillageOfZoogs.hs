module Arkham.Location.Cards.EnchantedWoodsVillageOfZoogs (enchantedWoodsVillageOfZoogs) where

import Arkham.GameValue
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Zoog))

newtype EnchantedWoodsVillageOfZoogs = EnchantedWoodsVillageOfZoogs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsVillageOfZoogs :: LocationCard EnchantedWoodsVillageOfZoogs
enchantedWoodsVillageOfZoogs = location EnchantedWoodsVillageOfZoogs Cards.enchantedWoodsVillageOfZoogs 3 (PerPlayer 1)

instance HasAbilities EnchantedWoodsVillageOfZoogs where
  getAbilities (EnchantedWoodsVillageOfZoogs a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemySpawns #after (be a) (EnemyWithTrait Zoog <> SwarmingEnemy <> NotEnemy IsSwarm)

instance RunMessage EnchantedWoodsVillageOfZoogs where
  runMessage msg l@(EnchantedWoodsVillageOfZoogs attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (spawnedEnemy -> enemy) _ -> do
      lead <- getLead
      push $ PlaceSwarmCards lead enemy 1
      pure l
    _ -> EnchantedWoodsVillageOfZoogs <$> liftRunMessage msg attrs
