module Arkham.Location.Cards.FirstNationalGroceryInTooDeep (
  firstNationalGroceryInTooDeep,
  FirstNationalGroceryInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype FirstNationalGroceryInTooDeep = FirstNationalGroceryInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstNationalGroceryInTooDeep :: LocationCard FirstNationalGroceryInTooDeep
firstNationalGroceryInTooDeep =
  locationWith
    FirstNationalGroceryInTooDeep
    Cards.firstNationalGroceryInTooDeep
    3
    (PerPlayer 2)
    connectsToAdjacent

instance HasAbilities FirstNationalGroceryInTooDeep where
  getAbilities (FirstNationalGroceryInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage FirstNationalGroceryInTooDeep where
  runMessage msg (FirstNationalGroceryInTooDeep attrs) = runQueueT $ case msg of
    _ -> FirstNationalGroceryInTooDeep <$> liftRunMessage msg attrs
