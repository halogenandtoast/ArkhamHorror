module Arkham.Enemy.Cards.Heretic_E
  ( heretic_E
  , Heretic_E(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Heretic_E = Heretic_E EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

heretic_E :: EnemyCard Heretic_E
heretic_E = enemy Heretic_E Cards.heretic_E (4, Static 2, 3) (1, 1)

instance RunMessage Heretic_E where
  runMessage msg (Heretic_E attrs) =
    Heretic_E <$> runMessage msg attrs
