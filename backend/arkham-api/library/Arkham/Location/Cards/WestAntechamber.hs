module Arkham.Location.Cards.WestAntechamber (westAntechamber) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WestAntechamber = WestAntechamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westAntechamber :: LocationCard WestAntechamber
westAntechamber = location WestAntechamber Cards.westAntechamber 3 (Static 1)

-- TODO: abilities

instance RunMessage WestAntechamber where
  runMessage msg (WestAntechamber attrs) = runQueueT $ WestAntechamber <$> liftRunMessage msg attrs
