module Arkham.Enemy.Cards.PitWarden (pitWarden) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Zone

newtype PitWarden = PitWarden EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pitWarden :: EnemyCard PitWarden
pitWarden =
  enemyWith PitWarden Cards.pitWarden (4, Static 4, 1) (1, 1)
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt (LocationWithEnemy $ enemyIs Enemies.yig)
      , SpawnPlaced (OutOfPlay PursuitZone)
      ]

instance RunMessage PitWarden where
  runMessage msg (PitWarden attrs) = PitWarden <$> runMessage msg attrs
