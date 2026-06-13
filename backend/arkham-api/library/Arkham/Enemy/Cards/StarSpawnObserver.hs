module Arkham.Enemy.Cards.StarSpawnObserver (starSpawnObserver) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype StarSpawnObserver = StarSpawnObserver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starSpawnObserver :: EnemyCard StarSpawnObserver
starSpawnObserver = enemy StarSpawnObserver Cards.starSpawnObserver (4, Static 4, 3) (1, 1)

-- TODO: abilities
instance RunMessage StarSpawnObserver where
  runMessage msg (StarSpawnObserver attrs) = runQueueT $ StarSpawnObserver <$> liftRunMessage msg attrs
