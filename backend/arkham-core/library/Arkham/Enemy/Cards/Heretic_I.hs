module Arkham.Enemy.Cards.Heretic_I
  ( heretic_I
  , Heretic_I(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Heretic_I = Heretic_I EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

heretic_I :: EnemyCard Heretic_I
heretic_I = enemy Heretic_I Cards.heretic_I (0, Static 1, 0) (0, 0)

instance RunMessage Heretic_I where
  runMessage msg (Heretic_I attrs) =
    Heretic_I <$> runMessage msg attrs
