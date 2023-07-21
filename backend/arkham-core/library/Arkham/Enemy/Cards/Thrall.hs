module Arkham.Enemy.Cards.Thrall (
  Thrall (..),
  thrall,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype Thrall = Thrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thrall :: EnemyCard Thrall
thrall =
  enemyWith
    Thrall
    Cards.thrall
    (2, Static 2, 2)
    (1, 1)
    (spawnAtL ?~ SpawnLocation (LocationWithMostClues Anywhere))

instance RunMessage Thrall where
  runMessage msg (Thrall attrs) = Thrall <$> runMessage msg attrs
