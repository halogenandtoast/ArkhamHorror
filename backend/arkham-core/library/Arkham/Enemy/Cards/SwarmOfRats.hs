module Arkham.Enemy.Cards.SwarmOfRats (
  SwarmOfRats (..),
  swarmOfRats,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SwarmOfRats = SwarmOfRats EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

swarmOfRats :: EnemyCard SwarmOfRats
swarmOfRats = enemy SwarmOfRats Cards.swarmOfRats (1, Static 1, 3) (1, 0)

instance RunMessage SwarmOfRats where
  runMessage msg (SwarmOfRats attrs) = SwarmOfRats <$> runMessage msg attrs
