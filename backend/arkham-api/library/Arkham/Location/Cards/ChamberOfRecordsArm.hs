module Arkham.Location.Cards.ChamberOfRecordsArm (chamberOfRecordsArm) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChamberOfRecordsArm = ChamberOfRecordsArm LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfRecordsArm :: LocationCard ChamberOfRecordsArm
chamberOfRecordsArm = location ChamberOfRecordsArm Cards.chamberOfRecordsArm 4 (Static 1)

-- TODO: abilities

instance RunMessage ChamberOfRecordsArm where
  runMessage msg (ChamberOfRecordsArm attrs) = runQueueT $ ChamberOfRecordsArm <$> liftRunMessage msg attrs
