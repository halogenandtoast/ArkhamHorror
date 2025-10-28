module Arkham.Enemy.Cards.SinisterAspirantC (sinisterAspirantC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SinisterAspirantC = SinisterAspirantC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sinisterAspirantC :: EnemyCard SinisterAspirantC
sinisterAspirantC = enemy SinisterAspirantC Cards.sinisterAspirantC (2, Static 3, 4) (0, 1)

instance RunMessage SinisterAspirantC where
  runMessage msg (SinisterAspirantC attrs) = runQueueT $ case msg of
    _ -> SinisterAspirantC <$> liftRunMessage msg attrs
