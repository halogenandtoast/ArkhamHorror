module Arkham.Asset.Assets.KeyLocusDefensiveBarrier (keyLocusDefensiveBarrier) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype KeyLocusDefensiveBarrier = KeyLocusDefensiveBarrier AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyLocusDefensiveBarrier :: AssetCard KeyLocusDefensiveBarrier
keyLocusDefensiveBarrier = asset KeyLocusDefensiveBarrier Cards.keyLocusDefensiveBarrier

instance RunMessage KeyLocusDefensiveBarrier where
  runMessage msg (KeyLocusDefensiveBarrier attrs) = runQueueT $ case msg of
    _ -> KeyLocusDefensiveBarrier <$> liftRunMessage msg attrs
