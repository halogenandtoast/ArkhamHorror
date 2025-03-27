module Arkham.Location.Cards.RooflessRampart (rooflessRampart) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RooflessRampart = RooflessRampart LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rooflessRampart :: LocationCard RooflessRampart
rooflessRampart = locationWith RooflessRampart Cards.rooflessRampart 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RooflessRampart where
  getAbilities (RooflessRampart attrs) =
    extendRevealed attrs []

instance RunMessage RooflessRampart where
  runMessage msg (RooflessRampart attrs) = runQueueT $ case msg of
    _ -> RooflessRampart <$> liftRunMessage msg attrs
