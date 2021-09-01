module Arkham.Types.Enemy.Cards.MarshGug
  ( marshGug
  , MarshGug(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Trait

newtype MarshGug = MarshGug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

marshGug :: EnemyCard MarshGug
marshGug = enemyWith
  MarshGug
  Cards.marshGug
  (3, Static 4, 3)
  (2, 1)
  (spawnAtL ?~ LocationWithTrait Bayou)

instance EnemyRunner env => RunMessage env MarshGug where
  runMessage msg (MarshGug attrs) = MarshGug <$> runMessage msg attrs
