module Arkham.Enemy.Cards.GhoulFromTheDepths
  ( GhoulFromTheDepths(..)
  , ghoulFromTheDepths
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype GhoulFromTheDepths = GhoulFromTheDepths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghoulFromTheDepths :: EnemyCard GhoulFromTheDepths
ghoulFromTheDepths = enemyWith
  GhoulFromTheDepths
  Cards.ghoulFromTheDepths
  (3, Static 4, 2)
  (1, 1)
  (spawnAtL ?~ LocationWithTitle "Bathroom")

instance RunMessage GhoulFromTheDepths where
  runMessage msg (GhoulFromTheDepths attrs) =
    GhoulFromTheDepths <$> runMessage msg attrs
