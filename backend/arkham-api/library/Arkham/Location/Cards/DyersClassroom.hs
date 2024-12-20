module Arkham.Location.Cards.DyersClassroom (dyersClassroom) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DyersClassroom = DyersClassroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dyersClassroom :: LocationCard DyersClassroom
dyersClassroom = location DyersClassroom Cards.dyersClassroom 5 (PerPlayer 2)

instance HasAbilities DyersClassroom where
  getAbilities (DyersClassroom attrs) =
    extendRevealed attrs []

instance RunMessage DyersClassroom where
  runMessage msg (DyersClassroom attrs) = runQueueT $ case msg of
    _ -> DyersClassroom <$> liftRunMessage msg attrs
