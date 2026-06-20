module Arkham.Enemy.Cards.MonstrousStarSpawn (monstrousStarSpawn) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MonstrousStarSpawn = MonstrousStarSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monstrousStarSpawn :: EnemyCard MonstrousStarSpawn
monstrousStarSpawn = enemy MonstrousStarSpawn Cards.monstrousStarSpawn

-- TODO: abilities
instance RunMessage MonstrousStarSpawn where
  runMessage msg (MonstrousStarSpawn attrs) = runQueueT $ MonstrousStarSpawn <$> liftRunMessage msg attrs
