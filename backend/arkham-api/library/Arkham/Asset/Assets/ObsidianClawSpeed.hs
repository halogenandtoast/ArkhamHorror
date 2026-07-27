module Arkham.Asset.Assets.ObsidianClawSpeed (obsidianClaw) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers

newtype ObsidianClawSpeed = ObsidianClawSpeed AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
obsidianClaw :: AssetCard ObsidianClawSpeed
obsidianClaw = asset ObsidianClawSpeed Cards.obsidianClaw

instance HasModifiersFor ObsidianClawSpeed where
  getModifiersFor (ObsidianClawSpeed a) = artifactModifiers a

instance HasAbilities ObsidianClawSpeed where
  getAbilities (ObsidianClawSpeed a) = [artifactAbility a 1]

instance RunMessage ObsidianClawSpeed where
  runMessage msg a@(ObsidianClawSpeed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handOffArtifact iid attrs
      pure a
    _ -> ObsidianClawSpeed <$> liftRunMessage msg attrs
