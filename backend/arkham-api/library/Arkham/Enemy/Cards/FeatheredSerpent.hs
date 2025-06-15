module Arkham.Enemy.Cards.FeatheredSerpent (featheredSerpent) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FeatheredSerpent = FeatheredSerpent EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

featheredSerpent :: EnemyCard FeatheredSerpent
featheredSerpent = enemy FeatheredSerpent Cards.featheredSerpent (0, Static 1, 0) (0, 0)

instance RunMessage FeatheredSerpent where
  runMessage msg (FeatheredSerpent attrs) = runQueueT $ case msg of
    _ -> FeatheredSerpent <$> liftRunMessage msg attrs
