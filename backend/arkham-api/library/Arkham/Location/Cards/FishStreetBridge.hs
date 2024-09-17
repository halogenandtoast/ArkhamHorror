module Arkham.Location.Cards.FishStreetBridge
  ( fishStreetBridge
  , FishStreetBridge(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FishStreetBridge = FishStreetBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishStreetBridge :: LocationCard FishStreetBridge
fishStreetBridge = location FishStreetBridge Cards.fishStreetBridge 2 (PerPlayer 1)

instance HasAbilities FishStreetBridge where
  getAbilities (FishStreetBridge attrs) =
    extendRevealed attrs []

instance RunMessage FishStreetBridge where
  runMessage msg (FishStreetBridge attrs) = runQueueT $ case msg of
    _ -> FishStreetBridge <$> liftRunMessage msg attrs
