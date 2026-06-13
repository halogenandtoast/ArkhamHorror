module Arkham.Asset.Assets.BarrierNodeEnergyShield (barrierNode) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype BarrierNodeEnergyShield = BarrierNodeEnergyShield AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: abilities
barrierNode :: AssetCard BarrierNodeEnergyShield
barrierNode = asset BarrierNodeEnergyShield Cards.barrierNode

instance RunMessage BarrierNodeEnergyShield where
  runMessage msg (BarrierNodeEnergyShield attrs) =
    runQueueT $ BarrierNodeEnergyShield <$> liftRunMessage msg attrs
