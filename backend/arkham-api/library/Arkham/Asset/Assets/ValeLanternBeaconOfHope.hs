module Arkham.Asset.Assets.ValeLanternBeaconOfHope (valeLanternBeaconOfHope) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ValeLanternBeaconOfHope = ValeLanternBeaconOfHope AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternBeaconOfHope :: AssetCard ValeLanternBeaconOfHope
valeLanternBeaconOfHope = asset ValeLanternBeaconOfHope Cards.valeLanternBeaconOfHope

instance RunMessage ValeLanternBeaconOfHope where
  runMessage msg (ValeLanternBeaconOfHope attrs) = runQueueT $ case msg of
    _ -> ValeLanternBeaconOfHope <$> liftRunMessage msg attrs
