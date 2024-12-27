module Arkham.Location.Cards.RockyLedge (rockyLedge) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RockyLedge = RockyLedge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rockyLedge :: LocationCard RockyLedge
rockyLedge = locationWith RockyLedge Cards.rockyLedge 4 (PerPlayer 1) (connectsToL .~ adjacentLocations)

instance HasAbilities RockyLedge where
  getAbilities (RockyLedge attrs) =
    extendRevealed attrs []

instance RunMessage RockyLedge where
  runMessage msg (RockyLedge attrs) = runQueueT $ case msg of
    _ -> RockyLedge <$> liftRunMessage msg attrs
