module Arkham.Asset.Assets.CashCart (cashCart) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype CashCart = CashCart AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cashCart :: AssetCard CashCart
cashCart = asset CashCart Cards.cashCart

instance RunMessage CashCart where
  runMessage msg (CashCart attrs) = runQueueT $ case msg of
    _ -> CashCart <$> liftRunMessage msg attrs
