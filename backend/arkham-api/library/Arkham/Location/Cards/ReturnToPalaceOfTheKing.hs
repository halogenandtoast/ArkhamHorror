module Arkham.Location.Cards.ReturnToPalaceOfTheKing (returnToPalaceOfTheKing) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToPalaceOfTheKing = ReturnToPalaceOfTheKing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToPalaceOfTheKing :: LocationCard ReturnToPalaceOfTheKing
returnToPalaceOfTheKing = location ReturnToPalaceOfTheKing Cards.returnToPalaceOfTheKing 1 (PerPlayer 1)

instance HasAbilities ReturnToPalaceOfTheKing where
  getAbilities (ReturnToPalaceOfTheKing attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToPalaceOfTheKing where
  runMessage msg (ReturnToPalaceOfTheKing attrs) = runQueueT $ case msg of
    _ -> ReturnToPalaceOfTheKing <$> liftRunMessage msg attrs
