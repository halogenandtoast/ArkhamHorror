module Arkham.Enemy.Cards.SecurityPatrolB (securityPatrolB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SecurityPatrolB = SecurityPatrolB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

securityPatrolB :: EnemyCard SecurityPatrolB
securityPatrolB = enemy SecurityPatrolB Cards.securityPatrolB (3, Static 2, 3) (1, 0)

instance RunMessage SecurityPatrolB where
  runMessage msg (SecurityPatrolB attrs) = runQueueT $ case msg of
    _ -> SecurityPatrolB <$> liftRunMessage msg attrs
