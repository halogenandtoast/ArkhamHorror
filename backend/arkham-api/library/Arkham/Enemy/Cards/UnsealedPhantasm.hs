module Arkham.Enemy.Cards.UnsealedPhantasm (unsealedPhantasm) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype UnsealedPhantasm = UnsealedPhantasm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

unsealedPhantasm :: EnemyCard UnsealedPhantasm
unsealedPhantasm = enemy UnsealedPhantasm Cards.unsealedPhantasm (5, Static 5, 4) (2, 2)

instance RunMessage UnsealedPhantasm where
  runMessage msg (UnsealedPhantasm attrs) = runQueueT $ case msg of
    _ -> UnsealedPhantasm <$> liftRunMessage msg attrs
