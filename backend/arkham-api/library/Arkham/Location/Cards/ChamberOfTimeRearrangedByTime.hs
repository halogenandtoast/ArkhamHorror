module Arkham.Location.Cards.ChamberOfTimeRearrangedByTime (chamberOfTimeRearrangedByTime) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChamberOfTimeRearrangedByTime = ChamberOfTimeRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTimeRearrangedByTime :: LocationCard ChamberOfTimeRearrangedByTime
chamberOfTimeRearrangedByTime =
  location ChamberOfTimeRearrangedByTime Cards.chamberOfTimeRearrangedByTime 4 (PerPlayer 2)
    & setLabel "chamberOfTime"

instance HasAbilities ChamberOfTimeRearrangedByTime where
  getAbilities (ChamberOfTimeRearrangedByTime attrs) =
    extendRevealed attrs []

instance RunMessage ChamberOfTimeRearrangedByTime where
  runMessage msg (ChamberOfTimeRearrangedByTime attrs) = runQueueT $ case msg of
    _ -> ChamberOfTimeRearrangedByTime <$> liftRunMessage msg attrs
