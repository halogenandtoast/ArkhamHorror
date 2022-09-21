module Arkham.Enemy.Cards.PadmaAmrita
  ( padmaAmrita
  , PadmaAmrita(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype PadmaAmrita = PadmaAmrita EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

padmaAmrita :: EnemyCard PadmaAmrita
padmaAmrita = enemy PadmaAmrita Cards.padmaAmrita (5, PerPlayer 3, 3) (0, 0)

instance RunMessage PadmaAmrita where
  runMessage msg (PadmaAmrita attrs) = PadmaAmrita <$> runMessage msg attrs
