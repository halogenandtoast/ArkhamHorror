module Arkham.Enemy.Cards.GangSoldier (gangSoldier) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GangSoldier = GangSoldier EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gangSoldier :: EnemyCard GangSoldier
gangSoldier = enemy GangSoldier Cards.gangSoldier (2, Static 2, 2) (1, 0)

instance RunMessage GangSoldier where
  runMessage msg (GangSoldier attrs) = runQueueT $ GangSoldier <$> liftRunMessage msg attrs
