module Arkham.Location.Cards.AlkalineForest (alkalineForest) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AlkalineForest = AlkalineForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alkalineForest :: LocationCard AlkalineForest
alkalineForest = locationWith AlkalineForest Cards.alkalineForest 2 (PerPlayer 2) connectsToAdjacent

instance HasAbilities AlkalineForest where
  getAbilities (AlkalineForest a) =
    extendRevealed a []

instance RunMessage AlkalineForest where
  runMessage msg (AlkalineForest attrs) = runQueueT $ case msg of
    _ -> AlkalineForest <$> liftRunMessage msg attrs
