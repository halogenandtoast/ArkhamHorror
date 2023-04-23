module Arkham.Enemy.Cards.PitWarden
  ( pitWarden
  , PitWarden(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Placement

newtype PitWarden = PitWarden EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pitWarden :: EnemyCard PitWarden
pitWarden = enemyWith
  PitWarden
  Cards.pitWarden
  (4, Static 4, 1)
  (1, 1)
  (spawnAtL ?~ SpawnAtFirst
    [ SpawnLocation (LocationWithEnemy $ enemyIs Enemies.yig)
    , SpawnPlaced (OutOfPlay PursuitZone)
    ]
  )

instance RunMessage PitWarden where
  runMessage msg (PitWarden attrs) = PitWarden <$> runMessage msg attrs
