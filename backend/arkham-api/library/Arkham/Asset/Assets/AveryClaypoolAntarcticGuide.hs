module Arkham.Asset.Assets.AveryClaypoolAntarcticGuide (
  averyClaypoolAntarcticGuide,
  AveryClaypoolAntarcticGuide (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype AveryClaypoolAntarcticGuide = AveryClaypoolAntarcticGuide AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

averyClaypoolAntarcticGuide :: AssetCard AveryClaypoolAntarcticGuide
averyClaypoolAntarcticGuide = allyWith AveryClaypoolAntarcticGuide Cards.averyClaypoolAntarcticGuide (3, 3) noSlots

instance RunMessage AveryClaypoolAntarcticGuide where
  runMessage msg (AveryClaypoolAntarcticGuide attrs) = runQueueT $ case msg of
    _ -> AveryClaypoolAntarcticGuide <$> liftRunMessage msg attrs
