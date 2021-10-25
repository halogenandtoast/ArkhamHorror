module Arkham.Types.Enemy.Cards.Thrall
  ( Thrall(..)
  , thrall
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher

newtype Thrall = Thrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thrall :: EnemyCard Thrall
thrall = enemyWith
  Thrall
  Cards.thrall
  (2, Static 2, 2)
  (1, 1)
  (spawnAtL ?~ LocationWithMostClues Anywhere)

instance EnemyRunner env => RunMessage env Thrall where
  runMessage msg (Thrall attrs) = Thrall <$> runMessage msg attrs
