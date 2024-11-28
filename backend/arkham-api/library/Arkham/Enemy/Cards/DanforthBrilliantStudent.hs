module Arkham.Enemy.Cards.DanforthBrilliantStudent
  ( danforthBrilliantStudent
  , DanforthBrilliantStudent(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DanforthBrilliantStudent = DanforthBrilliantStudent EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

danforthBrilliantStudent :: EnemyCard DanforthBrilliantStudent
danforthBrilliantStudent = enemy DanforthBrilliantStudent Cards.danforthBrilliantStudent (0, Static 1, 0) (0, 0)

instance RunMessage DanforthBrilliantStudent where
  runMessage msg (DanforthBrilliantStudent attrs) = runQueueT $ case msg of
    _ -> DanforthBrilliantStudent <$> liftRunMessage msg attrs
