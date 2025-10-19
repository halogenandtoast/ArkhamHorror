module Arkham.Location.Cards.GrandBazaarPublicBaths (grandBazaarPublicBaths) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBazaarPublicBaths = GrandBazaarPublicBaths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarPublicBaths :: LocationCard GrandBazaarPublicBaths
grandBazaarPublicBaths = location GrandBazaarPublicBaths Cards.grandBazaarPublicBaths 2 (PerPlayer 1)

instance HasAbilities GrandBazaarPublicBaths where
  getAbilities (GrandBazaarPublicBaths attrs) =
    extendRevealed attrs []

instance RunMessage GrandBazaarPublicBaths where
  runMessage msg (GrandBazaarPublicBaths attrs) = runQueueT $ case msg of
    _ -> GrandBazaarPublicBaths <$> liftRunMessage msg attrs
