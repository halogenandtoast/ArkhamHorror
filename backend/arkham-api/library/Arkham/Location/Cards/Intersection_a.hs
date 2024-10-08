module Arkham.Location.Cards.Intersection_a (
  intersection_a,
  Intersection_a (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Intersection_a = Intersection_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intersection_a :: LocationCard Intersection_a
intersection_a =
  locationWith Intersection_a Cards.intersection_a 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities Intersection_a where
  getAbilities (Intersection_a attrs) =
    extendRevealed attrs []

instance RunMessage Intersection_a where
  runMessage msg (Intersection_a attrs) = runQueueT $ case msg of
    _ -> Intersection_a <$> liftRunMessage msg attrs
