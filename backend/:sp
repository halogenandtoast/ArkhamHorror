module Arkham.Enemy.Cards.MemoryOfAHuntGoneAwry (memoryOfAHuntGoneAwry) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfAHuntGoneAwry = MemoryOfAHuntGoneAwry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfAHuntGoneAwry :: EnemyCard MemoryOfAHuntGoneAwry
memoryOfAHuntGoneAwry = enemy MemoryOfAHuntGoneAwry Cards.memoryOfAHuntGoneAwry (5, PerPlayer 3, 2) (1, 1)

instance RunMessage MemoryOfAHuntGoneAwry where
  runMessage msg (MemoryOfAHuntGoneAwry attrs) = runQueueT $ case msg of
    _ -> MemoryOfAHuntGoneAwry <$> liftRunMessage msg attrs
