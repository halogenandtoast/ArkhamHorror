module Arkham.Types.Enemy.Cards.DianneDevine
  ( dianneDevine
  , DianneDevine(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype DianneDevine = DianneDevine EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dianneDevine :: EnemyCard DianneDevine
dianneDevine = enemy DianneDevine Cards.dianneDevine (2, Static 3, 2) (0, 0)

instance EnemyRunner env => RunMessage env DianneDevine where
  runMessage msg (DianneDevine attrs) = DianneDevine <$> runMessage msg attrs
