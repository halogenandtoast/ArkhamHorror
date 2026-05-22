module Arkham.Asset.Assets.CrystalRemainsTheFather (crystalRemainsTheFather) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype CrystalRemainsTheFather = CrystalRemainsTheFather AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalRemainsTheFather :: AssetCard CrystalRemainsTheFather
crystalRemainsTheFather = asset CrystalRemainsTheFather Cards.crystalRemainsTheFather

instance RunMessage CrystalRemainsTheFather where
  runMessage msg (CrystalRemainsTheFather attrs) = runQueueT $ case msg of
    _ -> CrystalRemainsTheFather <$> liftRunMessage msg attrs
