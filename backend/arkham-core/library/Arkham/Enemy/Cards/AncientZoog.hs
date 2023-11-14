module Arkham.Enemy.Cards.AncientZoog
  ( ancientZoog
  , AncientZoog(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype AncientZoog = AncientZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ancientZoog :: EnemyCard AncientZoog
ancientZoog = enemy AncientZoog Cards.ancientZoog (3, Static 3, 3) (1, 1)

instance RunMessage AncientZoog where
  runMessage msg (AncientZoog attrs) =
    AncientZoog <$> runMessage msg attrs
