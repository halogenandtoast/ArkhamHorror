module Arkham.Enemy.Cards.SecurityPatrolA (securityPatrolA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SecurityPatrolA = SecurityPatrolA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

securityPatrolA :: EnemyCard SecurityPatrolA
securityPatrolA = enemy SecurityPatrolA Cards.securityPatrolA (3, Static 2, 3) (1, 0)

instance RunMessage SecurityPatrolA where
  runMessage msg (SecurityPatrolA attrs) = runQueueT $ case msg of
    _ -> SecurityPatrolA <$> liftRunMessage msg attrs
