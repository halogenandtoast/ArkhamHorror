module Arkham.Location.Cards.ArkhamWoodsPlaceOfPower (arkhamWoodsPlaceOfPower) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ArkhamWoodsPlaceOfPower = ArkhamWoodsPlaceOfPower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsPlaceOfPower :: LocationCard ArkhamWoodsPlaceOfPower
arkhamWoodsPlaceOfPower = location ArkhamWoodsPlaceOfPower Cards.arkhamWoodsPlaceOfPower 3 (PerPlayer 1)

instance HasAbilities ArkhamWoodsPlaceOfPower where
  getAbilities (ArkhamWoodsPlaceOfPower attrs) =
    extendRevealed attrs []

instance RunMessage ArkhamWoodsPlaceOfPower where
  runMessage msg (ArkhamWoodsPlaceOfPower attrs) = runQueueT $ case msg of
    _ -> ArkhamWoodsPlaceOfPower <$> liftRunMessage msg attrs
