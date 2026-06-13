module Arkham.Asset.Assets.PlumbTheDepthsMasteredAcumen (plumbTheDepthsCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype PlumbTheDepthsMasteredAcumen = PlumbTheDepthsMasteredAcumen AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
plumbTheDepthsCompleted :: AssetCard PlumbTheDepthsMasteredAcumen
plumbTheDepthsCompleted = asset PlumbTheDepthsMasteredAcumen Cards.plumbTheDepthsCompleted

instance RunMessage PlumbTheDepthsMasteredAcumen where
  runMessage msg (PlumbTheDepthsMasteredAcumen attrs) =
    runQueueT $ PlumbTheDepthsMasteredAcumen <$> liftRunMessage msg attrs
