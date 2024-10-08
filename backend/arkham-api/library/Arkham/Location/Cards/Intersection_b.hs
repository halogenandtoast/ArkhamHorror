module Arkham.Location.Cards.Intersection_b (
  intersection_b,
  Intersection_b (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Intersection_b = Intersection_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intersection_b :: LocationCard Intersection_b
intersection_b =
  locationWith Intersection_b Cards.intersection_b 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities Intersection_b where
  getAbilities (Intersection_b attrs) =
    extendRevealed attrs []

instance RunMessage Intersection_b where
  runMessage msg (Intersection_b attrs) = runQueueT $ case msg of
    _ -> Intersection_b <$> liftRunMessage msg attrs
