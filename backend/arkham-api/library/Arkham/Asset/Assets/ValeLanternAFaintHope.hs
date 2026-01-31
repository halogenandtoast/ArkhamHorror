module Arkham.Asset.Assets.ValeLanternAFaintHope (valeLanternAFaintHope) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ValeLanternAFaintHope = ValeLanternAFaintHope AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternAFaintHope :: AssetCard ValeLanternAFaintHope
valeLanternAFaintHope = asset ValeLanternAFaintHope Cards.valeLanternAFaintHope

instance RunMessage ValeLanternAFaintHope where
  runMessage msg (ValeLanternAFaintHope attrs) = runQueueT $ case msg of
    _ -> ValeLanternAFaintHope <$> liftRunMessage msg attrs
