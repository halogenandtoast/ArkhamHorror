module Arkham.Location.Cards.InnsmouthSquareInTooDeep (
  innsmouthSquareInTooDeep,
  InnsmouthSquareInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype InnsmouthSquareInTooDeep = InnsmouthSquareInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthSquareInTooDeep :: LocationCard InnsmouthSquareInTooDeep
innsmouthSquareInTooDeep =
  locationWith
    InnsmouthSquareInTooDeep
    Cards.innsmouthSquareInTooDeep
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities InnsmouthSquareInTooDeep where
  getAbilities (InnsmouthSquareInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage InnsmouthSquareInTooDeep where
  runMessage msg (InnsmouthSquareInTooDeep attrs) = runQueueT $ case msg of
    _ -> InnsmouthSquareInTooDeep <$> liftRunMessage msg attrs
