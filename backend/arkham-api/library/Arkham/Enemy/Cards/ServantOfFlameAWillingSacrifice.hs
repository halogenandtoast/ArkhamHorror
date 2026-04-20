module Arkham.Enemy.Cards.ServantOfFlameAWillingSacrifice (servantOfFlameAWillingSacrifice) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ServantOfFlameAWillingSacrifice = ServantOfFlameAWillingSacrifice EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

servantOfFlameAWillingSacrifice :: EnemyCard ServantOfFlameAWillingSacrifice
servantOfFlameAWillingSacrifice =
  enemy ServantOfFlameAWillingSacrifice Cards.servantOfFlameAWillingSacrifice (4, PerPlayer 5, 4) (2, 2)

instance RunMessage ServantOfFlameAWillingSacrifice where
  runMessage msg (ServantOfFlameAWillingSacrifice attrs) = ServantOfFlameAWillingSacrifice <$> runMessage msg attrs
