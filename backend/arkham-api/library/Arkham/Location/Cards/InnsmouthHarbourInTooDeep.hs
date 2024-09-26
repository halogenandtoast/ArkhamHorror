module Arkham.Location.Cards.InnsmouthHarbourInTooDeep (
  innsmouthHarbourInTooDeep,
  InnsmouthHarbourInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype InnsmouthHarbourInTooDeep = InnsmouthHarbourInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthHarbourInTooDeep :: LocationCard InnsmouthHarbourInTooDeep
innsmouthHarbourInTooDeep =
  locationWith
    InnsmouthHarbourInTooDeep
    Cards.innsmouthHarbourInTooDeep
    1
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities InnsmouthHarbourInTooDeep where
  getAbilities (InnsmouthHarbourInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage InnsmouthHarbourInTooDeep where
  runMessage msg (InnsmouthHarbourInTooDeep attrs) = runQueueT $ case msg of
    _ -> InnsmouthHarbourInTooDeep <$> liftRunMessage msg attrs
