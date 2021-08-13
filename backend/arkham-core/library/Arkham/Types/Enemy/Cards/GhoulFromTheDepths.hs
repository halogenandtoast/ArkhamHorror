module Arkham.Types.Enemy.Cards.GhoulFromTheDepths
  ( GhoulFromTheDepths(..)
  , ghoulFromTheDepths
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher

newtype GhoulFromTheDepths = GhoulFromTheDepths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions)

ghoulFromTheDepths :: EnemyCard GhoulFromTheDepths
ghoulFromTheDepths = enemyWith
  GhoulFromTheDepths
  Cards.ghoulFromTheDepths
  (3, Static 4, 2)
  (1, 1)
  (spawnAtL ?~ LocationWithTitle "Bathroom")

instance (EnemyRunner env) => RunMessage env GhoulFromTheDepths where
  runMessage msg (GhoulFromTheDepths attrs) =
    GhoulFromTheDepths <$> runMessage msg attrs
