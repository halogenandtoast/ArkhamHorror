module Arkham.Location.Cards.NewChurchGreenInTooDeep (
  newChurchGreenInTooDeep,
  NewChurchGreenInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype NewChurchGreenInTooDeep = NewChurchGreenInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newChurchGreenInTooDeep :: LocationCard NewChurchGreenInTooDeep
newChurchGreenInTooDeep =
  locationWith
    NewChurchGreenInTooDeep
    Cards.newChurchGreenInTooDeep
    3
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities NewChurchGreenInTooDeep where
  getAbilities (NewChurchGreenInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage NewChurchGreenInTooDeep where
  runMessage msg (NewChurchGreenInTooDeep attrs) = runQueueT $ case msg of
    _ -> NewChurchGreenInTooDeep <$> liftRunMessage msg attrs
