module Arkham.Location.Cards.DimlyLitRoad_c (
  dimlyLitRoad_c,
  DimlyLitRoad_c (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DimlyLitRoad_c = DimlyLitRoad_c LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimlyLitRoad_c :: LocationCard DimlyLitRoad_c
dimlyLitRoad_c =
  locationWith DimlyLitRoad_c Cards.dimlyLitRoad_c 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DimlyLitRoad_c where
  getAbilities (DimlyLitRoad_c attrs) =
    extendRevealed attrs []

instance RunMessage DimlyLitRoad_c where
  runMessage msg (DimlyLitRoad_c attrs) = runQueueT $ case msg of
    _ -> DimlyLitRoad_c <$> liftRunMessage msg attrs
