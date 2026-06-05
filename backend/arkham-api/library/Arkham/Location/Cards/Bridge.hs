module Arkham.Location.Cards.Bridge (bridge) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Bridge = Bridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bridge :: LocationCard Bridge
bridge = locationWith Bridge Cards.bridge 3 (PerPlayer 1) connectsToAdjacent

instance RunMessage Bridge where
  runMessage msg (Bridge attrs) = Bridge <$> runMessage msg attrs
