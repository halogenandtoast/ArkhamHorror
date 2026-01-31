module Arkham.Asset.Assets.ValeLanternExtinguishedLightBoon (valeLanternExtinguishedLightBoon) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ValeLanternExtinguishedLightBoon = ValeLanternExtinguishedLightBoon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternExtinguishedLightBoon :: AssetCard ValeLanternExtinguishedLightBoon
valeLanternExtinguishedLightBoon = asset ValeLanternExtinguishedLightBoon Cards.valeLanternExtinguishedLightBoon

instance RunMessage ValeLanternExtinguishedLightBoon where
  runMessage msg (ValeLanternExtinguishedLightBoon attrs) = runQueueT $ case msg of
    _ -> ValeLanternExtinguishedLightBoon <$> liftRunMessage msg attrs
