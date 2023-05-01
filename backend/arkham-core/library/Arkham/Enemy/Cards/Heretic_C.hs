module Arkham.Enemy.Cards.Heretic_C
  ( heretic_C
  , Heretic_C(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Heretic_C = Heretic_C EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

heretic_C :: EnemyCard Heretic_C
heretic_C = enemy Heretic_C Cards.heretic_C (4, Static 2, 3) (1, 1)

instance RunMessage Heretic_C where
  runMessage msg (Heretic_C attrs) =
    Heretic_C <$> runMessage msg attrs
