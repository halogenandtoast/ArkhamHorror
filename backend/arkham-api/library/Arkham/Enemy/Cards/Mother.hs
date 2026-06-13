module Arkham.Enemy.Cards.Mother (mother) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype Mother = Mother EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mother :: EnemyCard Mother
mother = enemy Mother Cards.mother (4, Static 8, 1) (1, 1)

-- TODO: abilities
instance RunMessage Mother where
  runMessage msg (Mother attrs) = runQueueT $ Mother <$> liftRunMessage msg attrs
