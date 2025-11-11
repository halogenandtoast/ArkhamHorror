module Arkham.Enemy.Cards.DimensionalDisplacerA (dimensionalDisplacerA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DimensionalDisplacerA = DimensionalDisplacerA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalDisplacerA :: EnemyCard DimensionalDisplacerA
dimensionalDisplacerA = enemy DimensionalDisplacerA Cards.dimensionalDisplacerA (5, Static 2, 3) (0, 2)

instance RunMessage DimensionalDisplacerA where
  runMessage msg (DimensionalDisplacerA attrs) = runQueueT $ case msg of
    _ -> DimensionalDisplacerA <$> liftRunMessage msg attrs
