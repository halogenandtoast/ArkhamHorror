module Arkham.Enemy.Cards.SecurityPatrolC (securityPatrolC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SecurityPatrolC = SecurityPatrolC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

securityPatrolC :: EnemyCard SecurityPatrolC
securityPatrolC = enemy SecurityPatrolC Cards.securityPatrolC (3, Static 2, 3) (1, 0)

instance RunMessage SecurityPatrolC where
  runMessage msg (SecurityPatrolC attrs) = runQueueT $ case msg of
    _ -> SecurityPatrolC <$> liftRunMessage msg attrs
