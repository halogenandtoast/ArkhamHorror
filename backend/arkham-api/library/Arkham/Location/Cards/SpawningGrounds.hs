module Arkham.Location.Cards.SpawningGrounds (spawningGrounds) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SpawningGrounds = SpawningGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spawningGrounds :: LocationCard SpawningGrounds
spawningGrounds = location SpawningGrounds Cards.spawningGrounds 3 (Static 2)

-- TODO: abilities

instance RunMessage SpawningGrounds where
  runMessage msg (SpawningGrounds attrs) = runQueueT $ SpawningGrounds <$> liftRunMessage msg attrs
