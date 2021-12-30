module Arkham.Enemy.Cards.SwarmOfRats
  ( SwarmOfRats(..)
  , swarmOfRats
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Attrs

newtype SwarmOfRats = SwarmOfRats EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

swarmOfRats :: EnemyCard SwarmOfRats
swarmOfRats = enemy SwarmOfRats Cards.swarmOfRats (1, Static 1, 3) (1, 0)

instance EnemyRunner env => RunMessage env SwarmOfRats where
  runMessage msg (SwarmOfRats attrs) = SwarmOfRats <$> runMessage msg attrs
