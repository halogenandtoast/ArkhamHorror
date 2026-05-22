module Arkham.Asset.Assets.CrystalRemainsTheChild (crystalRemainsTheChild) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype CrystalRemainsTheChild = CrystalRemainsTheChild AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalRemainsTheChild :: AssetCard CrystalRemainsTheChild
crystalRemainsTheChild = asset CrystalRemainsTheChild Cards.crystalRemainsTheChild

instance RunMessage CrystalRemainsTheChild where
  runMessage msg (CrystalRemainsTheChild attrs) = runQueueT $ case msg of
    _ -> CrystalRemainsTheChild <$> liftRunMessage msg attrs
