module Arkham.Enemy.Cards.CoralStarSpawn (coralStarSpawn) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CoralStarSpawn = CoralStarSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coralStarSpawn :: EnemyCard CoralStarSpawn
coralStarSpawn = enemy CoralStarSpawn Cards.coralStarSpawn

-- TODO: abilities
instance RunMessage CoralStarSpawn where
  runMessage msg (CoralStarSpawn attrs) = runQueueT $ CoralStarSpawn <$> liftRunMessage msg attrs
