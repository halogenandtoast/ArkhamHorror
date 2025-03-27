module Arkham.Location.Cards.CylindricalTower (cylindricalTower) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CylindricalTower = CylindricalTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cylindricalTower :: LocationCard CylindricalTower
cylindricalTower = locationWith CylindricalTower Cards.cylindricalTower 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CylindricalTower where
  getAbilities (CylindricalTower attrs) =
    extendRevealed attrs []

instance RunMessage CylindricalTower where
  runMessage msg (CylindricalTower attrs) = runQueueT $ case msg of
    _ -> CylindricalTower <$> liftRunMessage msg attrs
