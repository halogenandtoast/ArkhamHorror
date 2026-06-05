module Arkham.Enemy.Cards.GraspingOoze (graspingOoze) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GraspingOoze = GraspingOoze EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

graspingOoze :: EnemyCard GraspingOoze
graspingOoze = enemy GraspingOoze Cards.graspingOoze (3, Static 5, 3) (0, 2)

instance RunMessage GraspingOoze where
  runMessage msg (GraspingOoze attrs) = GraspingOoze <$> runMessage msg attrs
