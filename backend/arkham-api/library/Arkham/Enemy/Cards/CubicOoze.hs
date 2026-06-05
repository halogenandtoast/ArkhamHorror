module Arkham.Enemy.Cards.CubicOoze (cubicOoze) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CubicOoze = CubicOoze EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cubicOoze :: EnemyCard CubicOoze
cubicOoze = enemy CubicOoze Cards.cubicOoze (1, Static 4, 4) (2, 0)

instance RunMessage CubicOoze where
  runMessage msg (CubicOoze attrs) = CubicOoze <$> runMessage msg attrs
