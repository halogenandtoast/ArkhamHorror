module Arkham.Location.Cards.ForkInTheRoad_b (
  forkInTheRoad_b,
  ForkInTheRoad_b (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForkInTheRoad_b = ForkInTheRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkInTheRoad_b :: LocationCard ForkInTheRoad_b
forkInTheRoad_b =
  locationWith ForkInTheRoad_b Cards.forkInTheRoad_b 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ForkInTheRoad_b where
  getAbilities (ForkInTheRoad_b attrs) =
    extendRevealed attrs []

instance RunMessage ForkInTheRoad_b where
  runMessage msg (ForkInTheRoad_b attrs) = runQueueT $ case msg of
    _ -> ForkInTheRoad_b <$> liftRunMessage msg attrs
