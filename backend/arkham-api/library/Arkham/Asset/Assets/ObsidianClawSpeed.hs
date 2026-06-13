module Arkham.Asset.Assets.ObsidianClawSpeed (obsidianClaw) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ObsidianClawSpeed = ObsidianClawSpeed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
obsidianClaw :: AssetCard ObsidianClawSpeed
obsidianClaw = asset ObsidianClawSpeed Cards.obsidianClaw

instance RunMessage ObsidianClawSpeed where
  runMessage msg (ObsidianClawSpeed attrs) =
    runQueueT $ ObsidianClawSpeed <$> liftRunMessage msg attrs
