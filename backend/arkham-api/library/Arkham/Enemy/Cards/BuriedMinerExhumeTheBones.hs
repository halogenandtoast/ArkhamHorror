module Arkham.Enemy.Cards.BuriedMinerExhumeTheBones (buriedMinerExhumeTheBones) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BuriedMinerExhumeTheBones = BuriedMinerExhumeTheBones EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

buriedMinerExhumeTheBones :: EnemyCard BuriedMinerExhumeTheBones
buriedMinerExhumeTheBones = enemy BuriedMinerExhumeTheBones Cards.buriedMinerExhumeTheBones (0, Static 1, 0) (0, 0)

instance RunMessage BuriedMinerExhumeTheBones where
  runMessage msg (BuriedMinerExhumeTheBones attrs) = runQueueT $ case msg of
    _ -> BuriedMinerExhumeTheBones <$> liftRunMessage msg attrs
