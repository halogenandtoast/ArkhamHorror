module Arkham.Location.Cards.CentralChamber (centralChamber) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CentralChamber = CentralChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centralChamber :: LocationCard CentralChamber
centralChamber = location CentralChamber Cards.centralChamber 4 (Static 3)

-- TODO: abilities

instance RunMessage CentralChamber where
  runMessage msg (CentralChamber attrs) = runQueueT $ CentralChamber <$> liftRunMessage msg attrs
