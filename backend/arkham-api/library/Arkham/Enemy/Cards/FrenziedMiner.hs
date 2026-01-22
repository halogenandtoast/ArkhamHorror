module Arkham.Enemy.Cards.FrenziedMiner (frenziedMiner) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FrenziedMiner = FrenziedMiner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

frenziedMiner :: EnemyCard FrenziedMiner
frenziedMiner = enemy FrenziedMiner Cards.frenziedMiner (0, Static 4, 0) (1, 0)

instance RunMessage FrenziedMiner where
  runMessage msg (FrenziedMiner attrs) = runQueueT $ case msg of
    _ -> FrenziedMiner <$> liftRunMessage msg attrs
