module Arkham.Enemy.Cards.DimensionalShamblerHunterFromBeyond (dimensionalShamblerHunterFromBeyond) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DimensionalShamblerHunterFromBeyond = DimensionalShamblerHunterFromBeyond EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalShamblerHunterFromBeyond :: EnemyCard DimensionalShamblerHunterFromBeyond
dimensionalShamblerHunterFromBeyond = enemy DimensionalShamblerHunterFromBeyond Cards.dimensionalShamblerHunterFromBeyond (6, Static 5, 2) (2, 2)

instance RunMessage DimensionalShamblerHunterFromBeyond where
  runMessage msg (DimensionalShamblerHunterFromBeyond attrs) = runQueueT $ case msg of
    _ -> DimensionalShamblerHunterFromBeyond <$> liftRunMessage msg attrs
