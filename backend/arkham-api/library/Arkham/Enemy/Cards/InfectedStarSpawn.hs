module Arkham.Enemy.Cards.InfectedStarSpawn (infectedStarSpawn) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype InfectedStarSpawn = InfectedStarSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infectedStarSpawn :: EnemyCard InfectedStarSpawn
infectedStarSpawn = enemy InfectedStarSpawn Cards.infectedStarSpawn (2, Static 3, 4) (0, 1)

-- TODO: abilities
instance RunMessage InfectedStarSpawn where
  runMessage msg (InfectedStarSpawn attrs) = runQueueT $ InfectedStarSpawn <$> liftRunMessage msg attrs
