module Arkham.Enemy.Cards.SinisterAspirantA (sinisterAspirantA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SinisterAspirantA = SinisterAspirantA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sinisterAspirantA :: EnemyCard SinisterAspirantA
sinisterAspirantA = enemy SinisterAspirantA Cards.sinisterAspirantA (2, Static 3, 4) (0, 1)

instance RunMessage SinisterAspirantA where
  runMessage msg (SinisterAspirantA attrs) = runQueueT $ case msg of
    _ -> SinisterAspirantA <$> liftRunMessage msg attrs
