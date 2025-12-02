module Arkham.Asset.Assets.KeyLocusDefensiveBarrier (keyLocusDefensiveBarrier) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Scenarios.DogsOfWar.Helpers (pattern IsKeyLocus)

newtype KeyLocusDefensiveBarrier = KeyLocusDefensiveBarrier AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyLocusDefensiveBarrier :: AssetCard KeyLocusDefensiveBarrier
keyLocusDefensiveBarrier = asset KeyLocusDefensiveBarrier Cards.keyLocusDefensiveBarrier

instance HasModifiersFor KeyLocusDefensiveBarrier where
  getModifiersFor (KeyLocusDefensiveBarrier a) = modifySelf a [IsKeyLocus]

instance RunMessage KeyLocusDefensiveBarrier where
  runMessage msg (KeyLocusDefensiveBarrier attrs) = runQueueT $ case msg of
    _ -> KeyLocusDefensiveBarrier <$> liftRunMessage msg attrs
