module Arkham.Location.Cards.CliffsideRoad_a (
  cliffsideRoad_a,
  CliffsideRoad_a (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CliffsideRoad_a = CliffsideRoad_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cliffsideRoad_a :: LocationCard CliffsideRoad_a
cliffsideRoad_a =
  locationWith CliffsideRoad_a Cards.cliffsideRoad_a 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities CliffsideRoad_a where
  getAbilities (CliffsideRoad_a attrs) =
    extendRevealed attrs []

instance RunMessage CliffsideRoad_a where
  runMessage msg (CliffsideRoad_a attrs) = runQueueT $ case msg of
    _ -> CliffsideRoad_a <$> liftRunMessage msg attrs
