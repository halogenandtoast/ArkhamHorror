module Arkham.Location.Cards.TheHouseOnWaterStreetInTooDeep (
  theHouseOnWaterStreetInTooDeep,
  TheHouseOnWaterStreetInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype TheHouseOnWaterStreetInTooDeep = TheHouseOnWaterStreetInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseOnWaterStreetInTooDeep :: LocationCard TheHouseOnWaterStreetInTooDeep
theHouseOnWaterStreetInTooDeep =
  locationWith
    TheHouseOnWaterStreetInTooDeep
    Cards.theHouseOnWaterStreetInTooDeep
    2
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities TheHouseOnWaterStreetInTooDeep where
  getAbilities (TheHouseOnWaterStreetInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage TheHouseOnWaterStreetInTooDeep where
  runMessage msg (TheHouseOnWaterStreetInTooDeep attrs) = runQueueT $ case msg of
    _ -> TheHouseOnWaterStreetInTooDeep <$> liftRunMessage msg attrs
