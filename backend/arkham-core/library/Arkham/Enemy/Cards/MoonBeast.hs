module Arkham.Enemy.Cards.MoonBeast
  ( moonBeast
  , MoonBeast(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype MoonBeast = MoonBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonBeast :: EnemyCard MoonBeast
moonBeast = enemy MoonBeast Cards.moonBeast (0, Static 1, 0) (0, 0)

instance RunMessage MoonBeast where
  runMessage msg (MoonBeast attrs) =
    MoonBeast <$> runMessage msg attrs
