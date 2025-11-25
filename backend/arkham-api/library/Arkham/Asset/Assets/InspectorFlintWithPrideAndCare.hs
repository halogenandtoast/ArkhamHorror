module Arkham.Asset.Assets.InspectorFlintWithPrideAndCare (inspectorFlintWithPrideAndCare) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype InspectorFlintWithPrideAndCare = InspectorFlintWithPrideAndCare AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inspectorFlintWithPrideAndCare :: AssetCard InspectorFlintWithPrideAndCare
inspectorFlintWithPrideAndCare = asset InspectorFlintWithPrideAndCare Cards.inspectorFlintWithPrideAndCare

instance RunMessage InspectorFlintWithPrideAndCare where
  runMessage msg (InspectorFlintWithPrideAndCare attrs) = runQueueT $ case msg of
    _ -> InspectorFlintWithPrideAndCare <$> liftRunMessage msg attrs
