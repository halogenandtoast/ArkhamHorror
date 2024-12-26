module Arkham.Asset.Assets.MiasmicCrystalStrangeEvidence (miasmicCrystalStrangeEvidence) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MiasmicCrystalStrangeEvidence = MiasmicCrystalStrangeEvidence AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miasmicCrystalStrangeEvidence :: AssetCard MiasmicCrystalStrangeEvidence
miasmicCrystalStrangeEvidence = asset MiasmicCrystalStrangeEvidence Cards.miasmicCrystalStrangeEvidence

instance RunMessage MiasmicCrystalStrangeEvidence where
  runMessage msg (MiasmicCrystalStrangeEvidence attrs) = runQueueT $ case msg of
    _ -> MiasmicCrystalStrangeEvidence <$> liftRunMessage msg attrs
