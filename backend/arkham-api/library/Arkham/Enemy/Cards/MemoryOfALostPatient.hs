module Arkham.Enemy.Cards.MemoryOfALostPatient (memoryOfALostPatient) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfALostPatient = MemoryOfALostPatient EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfALostPatient :: EnemyCard MemoryOfALostPatient
memoryOfALostPatient = enemy MemoryOfALostPatient Cards.memoryOfALostPatient (3, PerPlayer 4, 4) (1, 1)

instance RunMessage MemoryOfALostPatient where
  runMessage msg (MemoryOfALostPatient attrs) = runQueueT $ case msg of
    _ -> MemoryOfALostPatient <$> liftRunMessage msg attrs
