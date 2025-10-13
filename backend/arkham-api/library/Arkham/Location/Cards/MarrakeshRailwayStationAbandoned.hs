module Arkham.Location.Cards.MarrakeshRailwayStationAbandoned (marrakeshRailwayStationAbandoned) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MarrakeshRailwayStationAbandoned = MarrakeshRailwayStationAbandoned LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marrakeshRailwayStationAbandoned :: LocationCard MarrakeshRailwayStationAbandoned
marrakeshRailwayStationAbandoned = symbolLabel $ location MarrakeshRailwayStationAbandoned Cards.marrakeshRailwayStationAbandoned 2 (Static 0)

instance HasAbilities MarrakeshRailwayStationAbandoned where
  getAbilities (MarrakeshRailwayStationAbandoned attrs) =
    extendRevealed attrs []

instance RunMessage MarrakeshRailwayStationAbandoned where
  runMessage msg (MarrakeshRailwayStationAbandoned attrs) = runQueueT $ case msg of
    _ -> MarrakeshRailwayStationAbandoned <$> liftRunMessage msg attrs
