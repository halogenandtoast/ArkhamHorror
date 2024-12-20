module Arkham.Enemy.Cards.MemoryOfATerribleDiscovery (memoryOfATerribleDiscovery) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfATerribleDiscovery = MemoryOfATerribleDiscovery EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfATerribleDiscovery :: EnemyCard MemoryOfATerribleDiscovery
memoryOfATerribleDiscovery = enemy MemoryOfATerribleDiscovery Cards.memoryOfATerribleDiscovery (3, PerPlayer 4, 3) (1, 1)

instance RunMessage MemoryOfATerribleDiscovery where
  runMessage msg (MemoryOfATerribleDiscovery attrs) = runQueueT $ case msg of
    _ -> MemoryOfATerribleDiscovery <$> liftRunMessage msg attrs
