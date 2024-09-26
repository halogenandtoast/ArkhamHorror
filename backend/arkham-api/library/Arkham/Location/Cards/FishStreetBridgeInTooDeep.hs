module Arkham.Location.Cards.FishStreetBridgeInTooDeep (
  fishStreetBridgeInTooDeep,
  FishStreetBridgeInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype FishStreetBridgeInTooDeep = FishStreetBridgeInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishStreetBridgeInTooDeep :: LocationCard FishStreetBridgeInTooDeep
fishStreetBridgeInTooDeep =
  locationWith
    FishStreetBridgeInTooDeep
    Cards.fishStreetBridgeInTooDeep
    1
    (PerPlayer 2)
    connectsToAdjacent

instance HasAbilities FishStreetBridgeInTooDeep where
  getAbilities (FishStreetBridgeInTooDeep attrs) =
    extendRevealed attrs []

instance RunMessage FishStreetBridgeInTooDeep where
  runMessage msg (FishStreetBridgeInTooDeep attrs) = runQueueT $ case msg of
    _ -> FishStreetBridgeInTooDeep <$> liftRunMessage msg attrs
