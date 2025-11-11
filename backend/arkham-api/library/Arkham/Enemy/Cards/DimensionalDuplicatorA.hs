module Arkham.Enemy.Cards.DimensionalDuplicatorA (dimensionalDuplicatorA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DimensionalDuplicatorA = DimensionalDuplicatorA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalDuplicatorA :: EnemyCard DimensionalDuplicatorA
dimensionalDuplicatorA = enemy DimensionalDuplicatorA Cards.dimensionalDuplicatorA (3, Static 3, 3) (1, 1)

instance RunMessage DimensionalDuplicatorA where
  runMessage msg (DimensionalDuplicatorA attrs) = runQueueT $ case msg of
    _ -> DimensionalDuplicatorA <$> liftRunMessage msg attrs
