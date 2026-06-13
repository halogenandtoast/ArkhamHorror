module Arkham.Enemy.Cards.GrotesqueAmalgam (grotesqueAmalgam) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GrotesqueAmalgam = GrotesqueAmalgam EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grotesqueAmalgam :: EnemyCard GrotesqueAmalgam
grotesqueAmalgam = enemy GrotesqueAmalgam Cards.grotesqueAmalgam (4, Static 5, 2) (1, 1)

-- TODO: abilities
instance RunMessage GrotesqueAmalgam where
  runMessage msg (GrotesqueAmalgam attrs) = runQueueT $ GrotesqueAmalgam <$> liftRunMessage msg attrs
