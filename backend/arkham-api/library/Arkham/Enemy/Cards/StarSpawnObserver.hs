module Arkham.Enemy.Cards.StarSpawnObserver (starSpawnObserver) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype StarSpawnObserver = StarSpawnObserver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

starSpawnObserver :: EnemyCard StarSpawnObserver
starSpawnObserver = enemy StarSpawnObserver Cards.starSpawnObserver

-- TODO: abilities
instance RunMessage StarSpawnObserver where
  runMessage msg (StarSpawnObserver attrs) = runQueueT $ StarSpawnObserver <$> liftRunMessage msg attrs
