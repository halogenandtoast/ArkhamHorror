module Arkham.Location.Cards.TheKnottedTower (theKnottedTower) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheKnottedTower = TheKnottedTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKnottedTower :: LocationCard TheKnottedTower
theKnottedTower = symbolLabel $ locationWith TheKnottedTower Cards.theKnottedTower 0 (Static 0) connectsToAdjacent

instance HasAbilities TheKnottedTower where
  getAbilities (TheKnottedTower a) =
    extendRevealed a []

instance RunMessage TheKnottedTower where
  runMessage msg (TheKnottedTower attrs) = runQueueT $ case msg of
    _ -> TheKnottedTower <$> liftRunMessage msg attrs
