module Arkham.Enemy.Cards.GhoulMinion ( ghoulMinion,) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GhoulMinion = GhoulMinion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghoulMinion :: EnemyCard GhoulMinion
ghoulMinion = enemy GhoulMinion Cards.ghoulMinion (2, Static 2, 2) (1, 1)

instance RunMessage GhoulMinion where
  runMessage msg (GhoulMinion attrs) = GhoulMinion <$> runMessage msg attrs
