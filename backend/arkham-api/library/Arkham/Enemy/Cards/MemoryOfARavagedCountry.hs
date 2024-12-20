module Arkham.Enemy.Cards.MemoryOfARavagedCountry (memoryOfARavagedCountry) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfARavagedCountry = MemoryOfARavagedCountry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfARavagedCountry :: EnemyCard MemoryOfARavagedCountry
memoryOfARavagedCountry = enemy MemoryOfARavagedCountry Cards.memoryOfARavagedCountry (5, PerPlayer 3, 3) (2, 1)

instance RunMessage MemoryOfARavagedCountry where
  runMessage msg (MemoryOfARavagedCountry attrs) = runQueueT $ case msg of
    _ -> MemoryOfARavagedCountry <$> liftRunMessage msg attrs
