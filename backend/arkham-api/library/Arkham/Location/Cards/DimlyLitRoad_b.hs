module Arkham.Location.Cards.DimlyLitRoad_b (
  dimlyLitRoad_b,
  DimlyLitRoad_b (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DimlyLitRoad_b = DimlyLitRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimlyLitRoad_b :: LocationCard DimlyLitRoad_b
dimlyLitRoad_b =
  locationWith DimlyLitRoad_b Cards.dimlyLitRoad_b 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DimlyLitRoad_b where
  getAbilities (DimlyLitRoad_b attrs) =
    extendRevealed attrs []

instance RunMessage DimlyLitRoad_b where
  runMessage msg (DimlyLitRoad_b attrs) = runQueueT $ case msg of
    _ -> DimlyLitRoad_b <$> liftRunMessage msg attrs
