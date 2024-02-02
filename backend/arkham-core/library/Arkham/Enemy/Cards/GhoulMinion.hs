module Arkham.Enemy.Cards.GhoulMinion (
  ghoulMinion,
  GhoulMinion (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype GhoulMinion = GhoulMinion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

ghoulMinion :: EnemyCard GhoulMinion
ghoulMinion = enemy GhoulMinion Cards.ghoulMinion (2, Static 2, 2) (1, 1)

instance RunMessage GhoulMinion where
  runMessage msg (GhoulMinion attrs) = GhoulMinion <$> runMessage msg attrs
