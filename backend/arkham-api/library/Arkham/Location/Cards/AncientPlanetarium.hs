module Arkham.Location.Cards.AncientPlanetarium (ancientPlanetarium) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AncientPlanetarium = AncientPlanetarium LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientPlanetarium :: LocationCard AncientPlanetarium
ancientPlanetarium = locationWith AncientPlanetarium Cards.ancientPlanetarium 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AncientPlanetarium where
  getAbilities (AncientPlanetarium attrs) =
    extendRevealed attrs []

instance RunMessage AncientPlanetarium where
  runMessage msg (AncientPlanetarium attrs) = runQueueT $ case msg of
    _ -> AncientPlanetarium <$> liftRunMessage msg attrs
