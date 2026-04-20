module Arkham.Enemy.Cards.ServantOfFlameOnTheRun (servantOfFlameOnTheRun) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ServantOfFlameOnTheRun = ServantOfFlameOnTheRun EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

servantOfFlameOnTheRun :: EnemyCard ServantOfFlameOnTheRun
servantOfFlameOnTheRun =
  enemy ServantOfFlameOnTheRun Cards.servantOfFlameOnTheRun (4, PerPlayer 5, 4) (2, 2)

instance RunMessage ServantOfFlameOnTheRun where
  runMessage msg (ServantOfFlameOnTheRun attrs) = ServantOfFlameOnTheRun <$> runMessage msg attrs
