module Arkham.Enemy.Cards.StarVampire (starVampire) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype StarVampire = StarVampire EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starVampire :: EnemyCard StarVampire
starVampire = enemy StarVampire Cards.starVampire (4, Static 5, 1) (1, 0)

-- TODO: abilities
instance RunMessage StarVampire where
  runMessage msg (StarVampire attrs) = runQueueT $ StarVampire <$> liftRunMessage msg attrs
