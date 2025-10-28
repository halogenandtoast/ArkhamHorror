module Arkham.Location.Cards.GrandBazaarJewelersRoad (grandBazaarJewelersRoad) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBazaarJewelersRoad = GrandBazaarJewelersRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarJewelersRoad :: LocationCard GrandBazaarJewelersRoad
grandBazaarJewelersRoad = location GrandBazaarJewelersRoad Cards.grandBazaarJewelersRoad 3 (Static 2)

instance HasAbilities GrandBazaarJewelersRoad where
  getAbilities (GrandBazaarJewelersRoad attrs) =
    extendRevealed attrs []

instance RunMessage GrandBazaarJewelersRoad where
  runMessage msg (GrandBazaarJewelersRoad attrs) = runQueueT $ case msg of
    _ -> GrandBazaarJewelersRoad <$> liftRunMessage msg attrs
