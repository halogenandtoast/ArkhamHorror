module Arkham.Location.Cards.ControlStation (controlStation) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ControlStation = ControlStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Act 2 needs to change the connections
controlStation :: LocationCard ControlStation
controlStation = locationWith ControlStation Cards.controlStation 1 (PerPlayer 2) connectsToAdjacent

instance HasAbilities ControlStation where
  getAbilities (ControlStation a) =
    extendRevealed a []

instance RunMessage ControlStation where
  runMessage msg (ControlStation attrs) = runQueueT $ case msg of
    _ -> ControlStation <$> liftRunMessage msg attrs
