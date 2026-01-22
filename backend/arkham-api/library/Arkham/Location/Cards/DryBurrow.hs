module Arkham.Location.Cards.DryBurrow (dryBurrow) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DryBurrow = DryBurrow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dryBurrow :: LocationCard DryBurrow
dryBurrow = locationWith DryBurrow Cards.dryBurrow 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities DryBurrow where
  getAbilities (DryBurrow a) =
    extendRevealed a []

instance RunMessage DryBurrow where
  runMessage msg (DryBurrow attrs) = runQueueT $ case msg of
    _ -> DryBurrow <$> liftRunMessage msg attrs
