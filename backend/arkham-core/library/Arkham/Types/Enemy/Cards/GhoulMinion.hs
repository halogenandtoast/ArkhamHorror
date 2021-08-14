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
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghoulMinion :: EnemyCard GhoulMinion
ghoulMinion = enemy GhoulMinion Cards.ghoulMinion (2, Static 2, 2) (1, 1)

instance HasModifiersFor env GhoulMinion

instance ActionRunner env => HasAbilities env GhoulMinion where
  getAbilities i window (GhoulMinion attrs) = getAbilities i window attrs

instance (EnemyRunner env) => RunMessage env GhoulMinion where
  runMessage msg (GhoulMinion attrs) = GhoulMinion <$> runMessage msg attrs
