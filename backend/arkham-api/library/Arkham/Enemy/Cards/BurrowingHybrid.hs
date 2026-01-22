module Arkham.Enemy.Cards.BurrowingHybrid (burrowingHybrid) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BurrowingHybrid = BurrowingHybrid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

burrowingHybrid :: EnemyCard BurrowingHybrid
burrowingHybrid = enemy BurrowingHybrid Cards.burrowingHybrid (3, Static 3, 3) (1, 1)

instance RunMessage BurrowingHybrid where
  runMessage msg (BurrowingHybrid attrs) = runQueueT $ case msg of
    _ -> BurrowingHybrid <$> liftRunMessage msg attrs
