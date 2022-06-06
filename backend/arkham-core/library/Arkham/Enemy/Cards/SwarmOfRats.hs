module Arkham.Enemy.Cards.SwarmOfRats
  ( SwarmOfRats(..)
  , swarmOfRats
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype SwarmOfRats = SwarmOfRats EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

swarmOfRats :: EnemyCard SwarmOfRats
swarmOfRats = enemy SwarmOfRats Cards.swarmOfRats (1, Static 1, 3) (1, 0)

instance EnemyRunner env => RunMessage SwarmOfRats where
  runMessage msg (SwarmOfRats attrs) = SwarmOfRats <$> runMessage msg attrs
