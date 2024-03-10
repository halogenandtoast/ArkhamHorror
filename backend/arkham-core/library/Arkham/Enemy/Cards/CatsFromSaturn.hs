module Arkham.Enemy.Cards.CatsFromSaturn
  ( catsFromSaturn
  , CatsFromSaturn(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype CatsFromSaturn = CatsFromSaturn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

catsFromSaturn :: EnemyCard CatsFromSaturn
catsFromSaturn = enemy CatsFromSaturn Cards.catsFromSaturn (0, Static 1, 0) (0, 0)

instance RunMessage CatsFromSaturn where
  runMessage msg (CatsFromSaturn attrs) =
    CatsFromSaturn <$> runMessage msg attrs
