module Arkham.Enemy.Cards.Thrall (thrall) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype Thrall = Thrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thrall :: EnemyCard Thrall
thrall =
  enemyWith Thrall Cards.thrall (2, Static 2, 2) (1, 1)
    $ spawnAtL
    ?~ SpawnAt (LocationWithMostClues Anywhere)

instance RunMessage Thrall where
  runMessage msg (Thrall attrs) = Thrall <$> runMessage msg attrs
