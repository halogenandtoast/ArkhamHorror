module Arkham.Location.Cards.ArkhamWoodsHiddenPath (arkhamWoodsHiddenPath) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ArkhamWoodsHiddenPath = ArkhamWoodsHiddenPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsHiddenPath :: LocationCard ArkhamWoodsHiddenPath
arkhamWoodsHiddenPath = location ArkhamWoodsHiddenPath Cards.arkhamWoodsHiddenPath 2 (PerPlayer 1)

instance HasAbilities ArkhamWoodsHiddenPath where
  getAbilities (ArkhamWoodsHiddenPath attrs) =
    extendRevealed attrs []

instance RunMessage ArkhamWoodsHiddenPath where
  runMessage msg (ArkhamWoodsHiddenPath attrs) = runQueueT $ case msg of
    _ -> ArkhamWoodsHiddenPath <$> liftRunMessage msg attrs
