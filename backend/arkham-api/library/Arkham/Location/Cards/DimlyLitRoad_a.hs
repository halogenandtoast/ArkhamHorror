module Arkham.Location.Cards.DimlyLitRoad_a (
  dimlyLitRoad_a,
  DimlyLitRoad_a (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DimlyLitRoad_a = DimlyLitRoad_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimlyLitRoad_a :: LocationCard DimlyLitRoad_a
dimlyLitRoad_a =
  locationWith DimlyLitRoad_a Cards.dimlyLitRoad_a 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DimlyLitRoad_a where
  getAbilities (DimlyLitRoad_a attrs) =
    extendRevealed attrs []

instance RunMessage DimlyLitRoad_a where
  runMessage msg (DimlyLitRoad_a attrs) = runQueueT $ case msg of
    _ -> DimlyLitRoad_a <$> liftRunMessage msg attrs
