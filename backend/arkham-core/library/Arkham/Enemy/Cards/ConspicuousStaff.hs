module Arkham.Enemy.Cards.ConspicuousStaff
  ( conspicuousStaff
  , ConspicuousStaff(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype ConspicuousStaff = ConspicuousStaff EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

conspicuousStaff :: EnemyCard ConspicuousStaff
conspicuousStaff = enemy ConspicuousStaff Cards.conspicuousStaff (3, Static 2, 3) (1, 1)

instance RunMessage ConspicuousStaff where
  runMessage msg (ConspicuousStaff attrs) =
    ConspicuousStaff <$> runMessage msg attrs
