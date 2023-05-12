module Arkham.Enemy.Cards.MalevolentSpirit (
  malevolentSpirit,
  MalevolentSpirit (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype MalevolentSpirit = MalevolentSpirit EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

malevolentSpirit :: EnemyCard MalevolentSpirit
malevolentSpirit =
  enemyWith
    MalevolentSpirit
    Cards.malevolentSpirit
    (2, Static 2, 4)
    (0, 1)
    ( spawnAtL
        ?~ SpawnLocation
          (LocationMatchAny [LocationWithTitle "Chapel Attic", LocationWithTitle "Chapel Crypt"])
    )

instance RunMessage MalevolentSpirit where
  runMessage msg (MalevolentSpirit attrs) =
    MalevolentSpirit <$> runMessage msg attrs
