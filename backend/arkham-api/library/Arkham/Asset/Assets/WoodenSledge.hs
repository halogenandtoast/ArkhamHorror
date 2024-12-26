module Arkham.Asset.Assets.WoodenSledge (woodenSledge) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype WoodenSledge = WoodenSledge AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

woodenSledge :: AssetCard WoodenSledge
woodenSledge = asset WoodenSledge Cards.woodenSledge

instance RunMessage WoodenSledge where
  runMessage msg (WoodenSledge attrs) = runQueueT $ case msg of
    _ -> WoodenSledge <$> liftRunMessage msg attrs
