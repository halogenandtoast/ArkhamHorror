module Arkham.Location.Cards.ReturnToGrandGuignol (returnToGrandGuignol) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToGrandGuignol = ReturnToGrandGuignol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToGrandGuignol :: LocationCard ReturnToGrandGuignol
returnToGrandGuignol = location ReturnToGrandGuignol Cards.returnToGrandGuignol 5 (PerPlayer 1)

instance HasAbilities ReturnToGrandGuignol where
  getAbilities (ReturnToGrandGuignol attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToGrandGuignol where
  runMessage msg (ReturnToGrandGuignol attrs) = runQueueT $ case msg of
    _ -> ReturnToGrandGuignol <$> liftRunMessage msg attrs
