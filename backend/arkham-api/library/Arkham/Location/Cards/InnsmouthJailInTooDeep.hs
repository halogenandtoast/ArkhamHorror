module Arkham.Location.Cards.InnsmouthJailInTooDeep (
  innsmouthJailInTooDeep,
  InnsmouthJailInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype InnsmouthJailInTooDeep = InnsmouthJailInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthJailInTooDeep :: LocationCard InnsmouthJailInTooDeep
innsmouthJailInTooDeep =
  locationWith InnsmouthJailInTooDeep Cards.innsmouthJailInTooDeep 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities InnsmouthJailInTooDeep where
  getAbilities (InnsmouthJailInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage InnsmouthJailInTooDeep where
  runMessage msg (InnsmouthJailInTooDeep attrs) = runQueueT $ case msg of
    _ -> InnsmouthJailInTooDeep <$> liftRunMessage msg attrs
