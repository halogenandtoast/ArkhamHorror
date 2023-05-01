module Arkham.Enemy.Cards.MalevolantSpirit
  ( malevolantSpirit
  , MalevolantSpirit(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype MalevolantSpirit = MalevolantSpirit EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

malevolantSpirit :: EnemyCard MalevolantSpirit
malevolantSpirit = enemyWith MalevolantSpirit Cards.malevolantSpirit (2, Static 2, 4) (0, 1) (spawnAtL ?~ SpawnLocation (LocationMatchAny [LocationWithTitle "Chapel Attic", LocationWithTitle "Chapel Crypt"]))

instance RunMessage MalevolantSpirit where
  runMessage msg (MalevolantSpirit attrs) =
    MalevolantSpirit <$> runMessage msg attrs
