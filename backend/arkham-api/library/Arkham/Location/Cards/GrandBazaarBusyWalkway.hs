module Arkham.Location.Cards.GrandBazaarBusyWalkway (grandBazaarBusyWalkway) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBazaarBusyWalkway = GrandBazaarBusyWalkway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarBusyWalkway :: LocationCard GrandBazaarBusyWalkway
grandBazaarBusyWalkway = locationWith GrandBazaarBusyWalkway Cards.grandBazaarBusyWalkway 1 (PerPlayer 1) connectsToAdjacent

instance HasAbilities GrandBazaarBusyWalkway where
  getAbilities (GrandBazaarBusyWalkway attrs) =
    extendRevealed attrs []

instance RunMessage GrandBazaarBusyWalkway where
  runMessage msg (GrandBazaarBusyWalkway attrs) = runQueueT $ case msg of
    _ -> GrandBazaarBusyWalkway <$> liftRunMessage msg attrs
