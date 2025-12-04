module Arkham.Enemy.Cards.BuriedMinerALostMemento (buriedMinerALostMemento) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BuriedMinerALostMemento = BuriedMinerALostMemento EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

buriedMinerALostMemento :: EnemyCard BuriedMinerALostMemento
buriedMinerALostMemento = enemy BuriedMinerALostMemento Cards.buriedMinerALostMemento (0, Static 1, 0) (0, 0)

instance RunMessage BuriedMinerALostMemento where
  runMessage msg (BuriedMinerALostMemento attrs) = runQueueT $ case msg of
    _ -> BuriedMinerALostMemento <$> liftRunMessage msg attrs
