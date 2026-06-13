module Arkham.Asset.Assets.ObsidianClawPower (obsidianClawPower) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ObsidianClawPower = ObsidianClawPower AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
obsidianClawPower :: AssetCard ObsidianClawPower
obsidianClawPower = asset ObsidianClawPower Cards.obsidianClawPower

instance RunMessage ObsidianClawPower where
  runMessage msg (ObsidianClawPower attrs) =
    runQueueT $ ObsidianClawPower <$> liftRunMessage msg attrs
