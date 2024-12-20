module Arkham.Enemy.Cards.MemoryOfARegretfulVoyage (memoryOfARegretfulVoyage) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MemoryOfARegretfulVoyage = MemoryOfARegretfulVoyage EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

memoryOfARegretfulVoyage :: EnemyCard MemoryOfARegretfulVoyage
memoryOfARegretfulVoyage = enemy MemoryOfARegretfulVoyage Cards.memoryOfARegretfulVoyage (2, PerPlayer 5, 5) (0, 1)

instance RunMessage MemoryOfARegretfulVoyage where
  runMessage msg (MemoryOfARegretfulVoyage attrs) = runQueueT $ case msg of
    _ -> MemoryOfARegretfulVoyage <$> liftRunMessage msg attrs
