module Arkham.Location.Cards.ReturnToMontparnasse (returnToMontparnasse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToMontparnasse = ReturnToMontparnasse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToMontparnasse :: LocationCard ReturnToMontparnasse
returnToMontparnasse = location ReturnToMontparnasse Cards.returnToMontparnasse 1 (PerPlayer 1)

instance HasAbilities ReturnToMontparnasse where
  getAbilities (ReturnToMontparnasse attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToMontparnasse where
  runMessage msg (ReturnToMontparnasse attrs) = runQueueT $ case msg of
    _ -> ReturnToMontparnasse <$> liftRunMessage msg attrs
