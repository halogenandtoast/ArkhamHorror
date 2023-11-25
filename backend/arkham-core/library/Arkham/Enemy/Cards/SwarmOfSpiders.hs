module Arkham.Enemy.Cards.SwarmOfSpiders
  ( swarmOfSpiders
  , SwarmOfSpiders(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SwarmOfSpiders = SwarmOfSpiders EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

swarmOfSpiders :: EnemyCard SwarmOfSpiders
swarmOfSpiders = enemy SwarmOfSpiders Cards.swarmOfSpiders (1, Static 1, 0) (1, 0)

instance RunMessage SwarmOfSpiders where
  runMessage msg (SwarmOfSpiders attrs) =
    SwarmOfSpiders <$> runMessage msg attrs
