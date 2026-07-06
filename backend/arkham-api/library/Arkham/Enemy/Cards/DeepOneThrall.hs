module Arkham.Enemy.Cards.DeepOneThrall (deepOneThrall) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DeepOneThrall = DeepOneThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

deepOneThrall :: EnemyCard DeepOneThrall
deepOneThrall = enemy DeepOneThrall Cards.deepOneThrall

-- TODO: abilities
instance RunMessage DeepOneThrall where
  runMessage msg (DeepOneThrall attrs) = runQueueT $ DeepOneThrall <$> liftRunMessage msg attrs
