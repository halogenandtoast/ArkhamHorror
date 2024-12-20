module Arkham.Enemy.Cards.MemoryOfAMissingFather (memoryOfAMissingFather) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfAMissingFather = MemoryOfAMissingFather EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfAMissingFather :: EnemyCard MemoryOfAMissingFather
memoryOfAMissingFather = enemy MemoryOfAMissingFather Cards.memoryOfAMissingFather (3, PerPlayer 4, 4) (1, 1)

instance RunMessage MemoryOfAMissingFather where
  runMessage msg (MemoryOfAMissingFather attrs) = runQueueT $ case msg of
    _ -> MemoryOfAMissingFather <$> liftRunMessage msg attrs
