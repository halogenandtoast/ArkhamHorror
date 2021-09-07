module Arkham.Types.Enemy.Cards.JordanPerry
  ( jordanPerry
  , JordanPerry(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype JordanPerry = JordanPerry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

jordanPerry :: EnemyCard JordanPerry
jordanPerry = enemy JordanPerry Cards.jordanPerry (0, Static 1, 0) (0, 0)

instance EnemyRunner env => RunMessage env JordanPerry where
  runMessage msg (JordanPerry attrs) = JordanPerry <$> runMessage msg attrs
