module Arkham.Asset.Assets.MineralSpecimen (mineralSpecimen) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MineralSpecimen = MineralSpecimen AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mineralSpecimen :: AssetCard MineralSpecimen
mineralSpecimen = asset MineralSpecimen Cards.mineralSpecimen

instance RunMessage MineralSpecimen where
  runMessage msg (MineralSpecimen attrs) = runQueueT $ case msg of
    _ -> MineralSpecimen <$> liftRunMessage msg attrs
