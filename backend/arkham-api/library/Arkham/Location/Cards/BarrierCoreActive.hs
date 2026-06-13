module Arkham.Location.Cards.BarrierCoreActive (barrierCoreActive) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BarrierCoreActive = BarrierCoreActive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierCoreActive :: LocationCard BarrierCoreActive
barrierCoreActive = location BarrierCoreActive Cards.barrierCoreActive 4 (Static 0)

-- TODO: abilities

instance RunMessage BarrierCoreActive where
  runMessage msg (BarrierCoreActive attrs) = runQueueT $ BarrierCoreActive <$> liftRunMessage msg attrs
