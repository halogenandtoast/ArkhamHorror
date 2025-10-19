module Arkham.Location.Cards.GrandBazaarRooftopAccess (grandBazaarRooftopAccess) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandBazaarRooftopAccess = GrandBazaarRooftopAccess LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarRooftopAccess :: LocationCard GrandBazaarRooftopAccess
grandBazaarRooftopAccess = location GrandBazaarRooftopAccess Cards.grandBazaarRooftopAccess 4 (PerPlayer 1)

instance HasAbilities GrandBazaarRooftopAccess where
  getAbilities (GrandBazaarRooftopAccess attrs) =
    extendRevealed attrs []

instance RunMessage GrandBazaarRooftopAccess where
  runMessage msg (GrandBazaarRooftopAccess attrs) = runQueueT $ case msg of
    _ -> GrandBazaarRooftopAccess <$> liftRunMessage msg attrs
