module Arkham.Location.Cards.MarrakeshRailwayStation (marrakeshRailwayStation) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MarrakeshRailwayStation = MarrakeshRailwayStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marrakeshRailwayStation :: LocationCard MarrakeshRailwayStation
marrakeshRailwayStation = symbolLabel $ location MarrakeshRailwayStation Cards.marrakeshRailwayStation 3 (Static 0)

instance HasAbilities MarrakeshRailwayStation where
  getAbilities (MarrakeshRailwayStation attrs) =
    extendRevealed attrs []

instance RunMessage MarrakeshRailwayStation where
  runMessage msg (MarrakeshRailwayStation attrs) = runQueueT $ case msg of
    _ -> MarrakeshRailwayStation <$> liftRunMessage msg attrs
