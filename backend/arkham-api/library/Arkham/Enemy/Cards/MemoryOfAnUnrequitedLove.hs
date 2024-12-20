module Arkham.Enemy.Cards.MemoryOfAnUnrequitedLove (memoryOfAnUnrequitedLove) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfAnUnrequitedLove = MemoryOfAnUnrequitedLove EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfAnUnrequitedLove :: EnemyCard MemoryOfAnUnrequitedLove
memoryOfAnUnrequitedLove = enemy MemoryOfAnUnrequitedLove Cards.memoryOfAnUnrequitedLove (4, PerPlayer 3, 3) (1, 1)

instance RunMessage MemoryOfAnUnrequitedLove where
  runMessage msg (MemoryOfAnUnrequitedLove attrs) = runQueueT $ case msg of
    _ -> MemoryOfAnUnrequitedLove <$> liftRunMessage msg attrs
