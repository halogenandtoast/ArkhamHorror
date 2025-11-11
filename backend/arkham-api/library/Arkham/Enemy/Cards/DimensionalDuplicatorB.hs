module Arkham.Enemy.Cards.DimensionalDuplicatorB (dimensionalDuplicatorB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DimensionalDuplicatorB = DimensionalDuplicatorB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalDuplicatorB :: EnemyCard DimensionalDuplicatorB
dimensionalDuplicatorB = enemy DimensionalDuplicatorB Cards.dimensionalDuplicatorB (3, Static 3, 3) (1, 1)

instance RunMessage DimensionalDuplicatorB where
  runMessage msg (DimensionalDuplicatorB attrs) = runQueueT $ case msg of
    _ -> DimensionalDuplicatorB <$> liftRunMessage msg attrs
