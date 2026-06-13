module Arkham.Enemy.Cards.DeepOneThrall (deepOneThrall) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DeepOneThrall = DeepOneThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneThrall :: EnemyCard DeepOneThrall
deepOneThrall = enemy DeepOneThrall Cards.deepOneThrall (3, Static 2, 3) (1, 1)

-- TODO: abilities
instance RunMessage DeepOneThrall where
  runMessage msg (DeepOneThrall attrs) = runQueueT $ DeepOneThrall <$> liftRunMessage msg attrs
