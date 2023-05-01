module Arkham.Enemy.Cards.Heretic_G
  ( heretic_G
  , Heretic_G(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Heretic_G = Heretic_G EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

heretic_G :: EnemyCard Heretic_G
heretic_G = enemy Heretic_G Cards.heretic_G (4, Static 2, 3) (1, 1)

instance RunMessage Heretic_G where
  runMessage msg (Heretic_G attrs) =
    Heretic_G <$> runMessage msg attrs
