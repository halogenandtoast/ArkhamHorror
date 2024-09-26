module Arkham.Location.Cards.MarshRefineryInTooDeep (
  marshRefineryInTooDeep,
  MarshRefineryInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype MarshRefineryInTooDeep = MarshRefineryInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marshRefineryInTooDeep :: LocationCard MarshRefineryInTooDeep
marshRefineryInTooDeep = locationWith MarshRefineryInTooDeep Cards.marshRefineryInTooDeep 3 (Static 1) connectsToAdjacent

instance HasAbilities MarshRefineryInTooDeep where
  getAbilities (MarshRefineryInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage MarshRefineryInTooDeep where
  runMessage msg (MarshRefineryInTooDeep attrs) = runQueueT $ case msg of
    _ -> MarshRefineryInTooDeep <$> liftRunMessage msg attrs
