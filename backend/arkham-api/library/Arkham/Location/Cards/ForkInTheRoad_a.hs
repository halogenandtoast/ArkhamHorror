module Arkham.Location.Cards.ForkInTheRoad_a (
  forkInTheRoad_a,
  ForkInTheRoad_a (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForkInTheRoad_a = ForkInTheRoad_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkInTheRoad_a :: LocationCard ForkInTheRoad_a
forkInTheRoad_a =
  locationWith ForkInTheRoad_a Cards.forkInTheRoad_a 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ForkInTheRoad_a where
  getAbilities (ForkInTheRoad_a attrs) =
    extendRevealed attrs []

instance RunMessage ForkInTheRoad_a where
  runMessage msg (ForkInTheRoad_a attrs) = runQueueT $ case msg of
    _ -> ForkInTheRoad_a <$> liftRunMessage msg attrs
