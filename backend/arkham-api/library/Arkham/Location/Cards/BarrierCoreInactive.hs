module Arkham.Location.Cards.BarrierCoreInactive (barrierCoreInactive) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BarrierCoreInactive = BarrierCoreInactive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierCoreInactive :: LocationCard BarrierCoreInactive
barrierCoreInactive = location BarrierCoreInactive Cards.barrierCoreInactive 4 (Static 1)

-- TODO: abilities

instance RunMessage BarrierCoreInactive where
  runMessage msg (BarrierCoreInactive attrs) = runQueueT $ BarrierCoreInactive <$> liftRunMessage msg attrs
