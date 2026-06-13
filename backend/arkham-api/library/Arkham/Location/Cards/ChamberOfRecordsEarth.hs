module Arkham.Location.Cards.ChamberOfRecordsEarth (chamberOfRecordsEarth) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChamberOfRecordsEarth = ChamberOfRecordsEarth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfRecordsEarth :: LocationCard ChamberOfRecordsEarth
chamberOfRecordsEarth = location ChamberOfRecordsEarth Cards.chamberOfRecordsEarth 4 (Static 1)

-- TODO: abilities

instance RunMessage ChamberOfRecordsEarth where
  runMessage msg (ChamberOfRecordsEarth attrs) = runQueueT $ ChamberOfRecordsEarth <$> liftRunMessage msg attrs
