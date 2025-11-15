module Arkham.Enemy.Cards.FlyingPolyp (flyingPolyp) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype FlyingPolyp = FlyingPolyp EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

flyingPolyp :: EnemyCard FlyingPolyp
flyingPolyp = enemy FlyingPolyp Cards.flyingPolyp (2, Static 2, 2) (1, 0)

instance RunMessage FlyingPolyp where
  runMessage msg (FlyingPolyp attrs) = FlyingPolyp <$> runMessage msg attrs
