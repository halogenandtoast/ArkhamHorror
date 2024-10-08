module Arkham.Location.Cards.CliffsideRoad_b (
  cliffsideRoad_b,
  CliffsideRoad_b (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CliffsideRoad_b = CliffsideRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cliffsideRoad_b :: LocationCard CliffsideRoad_b
cliffsideRoad_b =
  locationWith CliffsideRoad_b Cards.cliffsideRoad_b 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities CliffsideRoad_b where
  getAbilities (CliffsideRoad_b attrs) =
    extendRevealed attrs []

instance RunMessage CliffsideRoad_b where
  runMessage msg (CliffsideRoad_b attrs) = runQueueT $ case msg of
    _ -> CliffsideRoad_b <$> liftRunMessage msg attrs
