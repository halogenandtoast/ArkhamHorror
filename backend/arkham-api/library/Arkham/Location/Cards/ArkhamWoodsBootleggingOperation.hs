module Arkham.Location.Cards.ArkhamWoodsBootleggingOperation (arkhamWoodsBootleggingOperation) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ArkhamWoodsBootleggingOperation = ArkhamWoodsBootleggingOperation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsBootleggingOperation :: LocationCard ArkhamWoodsBootleggingOperation
arkhamWoodsBootleggingOperation = location ArkhamWoodsBootleggingOperation Cards.arkhamWoodsBootleggingOperation 1 (PerPlayer 1)

instance HasAbilities ArkhamWoodsBootleggingOperation where
  getAbilities (ArkhamWoodsBootleggingOperation attrs) =
    extendRevealed attrs []

instance RunMessage ArkhamWoodsBootleggingOperation where
  runMessage msg (ArkhamWoodsBootleggingOperation attrs) = runQueueT $ case msg of
    _ -> ArkhamWoodsBootleggingOperation <$> liftRunMessage msg attrs
