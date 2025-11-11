module Arkham.Enemy.Cards.DimensionalDisplacerB (dimensionalDisplacerB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DimensionalDisplacerB = DimensionalDisplacerB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalDisplacerB :: EnemyCard DimensionalDisplacerB
dimensionalDisplacerB = enemy DimensionalDisplacerB Cards.dimensionalDisplacerB (5, Static 3, 2) (0, 2)

instance RunMessage DimensionalDisplacerB where
  runMessage msg (DimensionalDisplacerB attrs) = runQueueT $ case msg of
    _ -> DimensionalDisplacerB <$> liftRunMessage msg attrs
