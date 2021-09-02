module Arkham.Types.Enemy.Cards.GhoulMinion
  ( ghoulMinion
  , GhoulMinion(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GhoulMinion = GhoulMinion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghoulMinion :: EnemyCard GhoulMinion
ghoulMinion = enemy GhoulMinion Cards.ghoulMinion (2, Static 2, 2) (1, 1)

instance (EnemyRunner env) => RunMessage env GhoulMinion where
  runMessage msg (GhoulMinion attrs) = GhoulMinion <$> runMessage msg attrs
