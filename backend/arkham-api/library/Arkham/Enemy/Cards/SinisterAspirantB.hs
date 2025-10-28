module Arkham.Enemy.Cards.SinisterAspirantB (sinisterAspirantB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SinisterAspirantB = SinisterAspirantB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sinisterAspirantB :: EnemyCard SinisterAspirantB
sinisterAspirantB = enemy SinisterAspirantB Cards.sinisterAspirantB (2, Static 3, 4) (0, 1)

instance RunMessage SinisterAspirantB where
  runMessage msg (SinisterAspirantB attrs) = runQueueT $ case msg of
    _ -> SinisterAspirantB <$> liftRunMessage msg attrs
