module Arkham.Location.Cards.WaterTower (waterTower) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WaterTower = WaterTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

waterTower :: LocationCard WaterTower
waterTower = locationWith WaterTower Cards.waterTower 4 (PerPlayer 1) connectsToAdjacent

instance RunMessage WaterTower where
  runMessage msg (WaterTower attrs) = WaterTower <$> runMessage msg attrs
