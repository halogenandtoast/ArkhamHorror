module Arkham.Enemy.Cards.ScarletBeast (scarletBeast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ScarletBeast = ScarletBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

scarletBeast :: EnemyCard ScarletBeast
scarletBeast = enemy ScarletBeast Cards.scarletBeast (0, Static 1, 0) (0, 0)

instance RunMessage ScarletBeast where
  runMessage msg (ScarletBeast attrs) = runQueueT $ case msg of
    _ -> ScarletBeast <$> liftRunMessage msg attrs
