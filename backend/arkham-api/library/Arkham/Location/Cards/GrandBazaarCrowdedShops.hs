module Arkham.Location.Cards.GrandBazaarCrowdedShops (grandBazaarCrowdedShops) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBazaarCrowdedShops = GrandBazaarCrowdedShops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarCrowdedShops :: LocationCard GrandBazaarCrowdedShops
grandBazaarCrowdedShops = location GrandBazaarCrowdedShops Cards.grandBazaarCrowdedShops 3 (PerPlayer 1)

instance HasAbilities GrandBazaarCrowdedShops where
  getAbilities (GrandBazaarCrowdedShops attrs) =
    extendRevealed attrs []

instance RunMessage GrandBazaarCrowdedShops where
  runMessage msg (GrandBazaarCrowdedShops attrs) = runQueueT $ case msg of
    _ -> GrandBazaarCrowdedShops <$> liftRunMessage msg attrs
