module Arkham.Enemy.Cards.FortuneAndFolly.DimensionalDuplicatorB (dimensionalDuplicatorB) where

import Arkham.Enemy.CardDefs.FortuneAndFolly qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DimensionalDuplicatorB = DimensionalDuplicatorB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalDuplicatorB :: EnemyCard DimensionalDuplicatorB
dimensionalDuplicatorB = enemy DimensionalDuplicatorB Cards.dimensionalDuplicatorB

instance RunMessage DimensionalDuplicatorB where
  runMessage msg (DimensionalDuplicatorB attrs) = runQueueT $ case msg of
    _ -> DimensionalDuplicatorB <$> liftRunMessage msg attrs
