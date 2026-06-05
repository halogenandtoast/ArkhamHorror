module Arkham.Location.Cards.DesiccatedFarmland (desiccatedFarmland) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DesiccatedFarmland = DesiccatedFarmland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

desiccatedFarmland :: LocationCard DesiccatedFarmland
desiccatedFarmland = locationWith DesiccatedFarmland Cards.desiccatedFarmland 3 (PerPlayer 1) connectsToAdjacent

instance RunMessage DesiccatedFarmland where
  runMessage msg (DesiccatedFarmland attrs) = DesiccatedFarmland <$> runMessage msg attrs
