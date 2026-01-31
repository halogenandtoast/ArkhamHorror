module Arkham.Asset.Assets.ValeLanternExtinguishedLight (valeLanternExtinguishedLight) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ValeLanternExtinguishedLight = ValeLanternExtinguishedLight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternExtinguishedLight :: AssetCard ValeLanternExtinguishedLight
valeLanternExtinguishedLight = asset ValeLanternExtinguishedLight Cards.valeLanternExtinguishedLight

instance RunMessage ValeLanternExtinguishedLight where
  runMessage msg (ValeLanternExtinguishedLight attrs) = runQueueT $ case msg of
    _ -> ValeLanternExtinguishedLight <$> liftRunMessage msg attrs
