module Arkham.Location.Cards.ChamberOfTheTabletUnsealed (chamberOfTheTabletUnsealed) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ChamberOfTheTabletUnsealed = ChamberOfTheTabletUnsealed LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTheTabletUnsealed :: LocationCard ChamberOfTheTabletUnsealed
chamberOfTheTabletUnsealed = location ChamberOfTheTabletUnsealed Cards.chamberOfTheTabletUnsealed 3 (Static 2)

-- TODO: abilities

instance RunMessage ChamberOfTheTabletUnsealed where
  runMessage msg (ChamberOfTheTabletUnsealed attrs) = runQueueT $ ChamberOfTheTabletUnsealed <$> liftRunMessage msg attrs
