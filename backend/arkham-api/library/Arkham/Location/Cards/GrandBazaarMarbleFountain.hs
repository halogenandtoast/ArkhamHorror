module Arkham.Location.Cards.GrandBazaarMarbleFountain (grandBazaarMarbleFountain) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBazaarMarbleFountain = GrandBazaarMarbleFountain LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarMarbleFountain :: LocationCard GrandBazaarMarbleFountain
grandBazaarMarbleFountain = locationWith GrandBazaarMarbleFountain Cards.grandBazaarMarbleFountain 5 (Static 3) connectsToAdjacent

instance HasAbilities GrandBazaarMarbleFountain where
  getAbilities (GrandBazaarMarbleFountain attrs) =
    extendRevealed attrs []

instance RunMessage GrandBazaarMarbleFountain where
  runMessage msg (GrandBazaarMarbleFountain attrs) = runQueueT $ case msg of
    _ -> GrandBazaarMarbleFountain <$> liftRunMessage msg attrs
