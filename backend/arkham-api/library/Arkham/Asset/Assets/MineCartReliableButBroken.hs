module Arkham.Asset.Assets.MineCartReliableButBroken (mineCartReliableButBroken) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MineCartReliableButBroken = MineCartReliableButBroken AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mineCartReliableButBroken :: AssetCard MineCartReliableButBroken
mineCartReliableButBroken = asset MineCartReliableButBroken Cards.mineCartReliableButBroken

instance RunMessage MineCartReliableButBroken where
  runMessage msg (MineCartReliableButBroken attrs) = runQueueT $ case msg of
    _ -> MineCartReliableButBroken <$> liftRunMessage msg attrs
