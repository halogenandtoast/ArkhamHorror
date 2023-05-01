module Arkham.Enemy.Cards.Heretic_A
  ( heretic_A
  , Heretic_A(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Heretic_A = Heretic_A EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

heretic_A :: EnemyCard Heretic_A
heretic_A = enemy Heretic_A Cards.heretic_A (4, Static 2, 3) (1, 1)

instance RunMessage Heretic_A where
  runMessage msg (Heretic_A attrs) =
    Heretic_A <$> runMessage msg attrs
