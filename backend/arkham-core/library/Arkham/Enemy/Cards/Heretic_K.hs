module Arkham.Enemy.Cards.Heretic_K
  ( heretic_K
  , Heretic_K(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Heretic_K = Heretic_K EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

heretic_K :: EnemyCard Heretic_K
heretic_K = enemy Heretic_K Cards.heretic_K (4, Static 2, 3) (1, 1)

instance RunMessage Heretic_K where
  runMessage msg (Heretic_K attrs) =
    Heretic_K <$> runMessage msg attrs
