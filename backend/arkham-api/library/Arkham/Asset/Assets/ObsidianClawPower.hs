module Arkham.Asset.Assets.ObsidianClawPower (obsidianClawPower) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers

newtype ObsidianClawPower = ObsidianClawPower AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
obsidianClawPower :: AssetCard ObsidianClawPower
obsidianClawPower = asset ObsidianClawPower Cards.obsidianClawPower

instance HasModifiersFor ObsidianClawPower where
  getModifiersFor (ObsidianClawPower a) = artifactModifiers a

instance HasAbilities ObsidianClawPower where
  getAbilities (ObsidianClawPower a) = [artifactAbility a 1]

instance RunMessage ObsidianClawPower where
  runMessage msg a@(ObsidianClawPower attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handOffArtifact iid attrs
      pure a
    _ -> ObsidianClawPower <$> liftRunMessage msg attrs
