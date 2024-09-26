module Arkham.Location.Cards.ShorewardSlumsInTooDeep (
  shorewardSlumsInTooDeep,
  ShorewardSlumsInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype ShorewardSlumsInTooDeep = ShorewardSlumsInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shorewardSlumsInTooDeep :: LocationCard ShorewardSlumsInTooDeep
shorewardSlumsInTooDeep =
  locationWith
    ShorewardSlumsInTooDeep
    Cards.shorewardSlumsInTooDeep
    2
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities ShorewardSlumsInTooDeep where
  getAbilities (ShorewardSlumsInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage ShorewardSlumsInTooDeep where
  runMessage msg (ShorewardSlumsInTooDeep attrs) = runQueueT $ case msg of
    _ -> ShorewardSlumsInTooDeep <$> liftRunMessage msg attrs
