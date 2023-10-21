module Arkham.Enemy.Cards.OtherwordlyMeddler
  ( otherwordlyMeddler
  , OtherwordlyMeddler(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype OtherwordlyMeddler = OtherwordlyMeddler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

otherwordlyMeddler :: EnemyCard OtherwordlyMeddler
otherwordlyMeddler = enemy OtherwordlyMeddler Cards.otherwordlyMeddler (4, PerPlayer 5, 3) (2, 2)

instance RunMessage OtherwordlyMeddler where
  runMessage msg (OtherwordlyMeddler attrs) =
    OtherwordlyMeddler <$> runMessage msg attrs
