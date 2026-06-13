module Arkham.Asset.Assets.ShardOfYchlechtOtherworldlyRemnant (shardOfYchlecht) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ShardOfYchlechtOtherworldlyRemnant = ShardOfYchlechtOtherworldlyRemnant AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
shardOfYchlecht :: AssetCard ShardOfYchlechtOtherworldlyRemnant
shardOfYchlecht = asset ShardOfYchlechtOtherworldlyRemnant Cards.shardOfYchlecht

instance RunMessage ShardOfYchlechtOtherworldlyRemnant where
  runMessage msg (ShardOfYchlechtOtherworldlyRemnant attrs) =
    runQueueT $ ShardOfYchlechtOtherworldlyRemnant <$> liftRunMessage msg attrs
