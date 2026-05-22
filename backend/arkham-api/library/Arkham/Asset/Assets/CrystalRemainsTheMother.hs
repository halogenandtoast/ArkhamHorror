module Arkham.Asset.Assets.CrystalRemainsTheMother (crystalRemainsTheMother) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype CrystalRemainsTheMother = CrystalRemainsTheMother AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalRemainsTheMother :: AssetCard CrystalRemainsTheMother
crystalRemainsTheMother = asset CrystalRemainsTheMother Cards.crystalRemainsTheMother

instance RunMessage CrystalRemainsTheMother where
  runMessage msg (CrystalRemainsTheMother attrs) = runQueueT $ case msg of
    _ -> CrystalRemainsTheMother <$> liftRunMessage msg attrs
