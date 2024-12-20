module Arkham.Enemy.Cards.MemoryOfAnUnspeakableEvil (memoryOfAnUnspeakableEvil) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfAnUnspeakableEvil = MemoryOfAnUnspeakableEvil EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfAnUnspeakableEvil :: EnemyCard MemoryOfAnUnspeakableEvil
memoryOfAnUnspeakableEvil = enemy MemoryOfAnUnspeakableEvil Cards.memoryOfAnUnspeakableEvil (4, PerPlayer 3, 4) (0, 2)

instance RunMessage MemoryOfAnUnspeakableEvil where
  runMessage msg (MemoryOfAnUnspeakableEvil attrs) = runQueueT $ case msg of
    _ -> MemoryOfAnUnspeakableEvil <$> liftRunMessage msg attrs
