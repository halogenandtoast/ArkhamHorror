module Arkham.Enemy.Cards.SummonedBeast
  ( summonedBeast
  , SummonedBeast(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SummonedBeast = SummonedBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

summonedBeast :: EnemyCard SummonedBeast
summonedBeast = enemy SummonedBeast Cards.summonedBeast (5, PerPlayer 6, 2) (2, 2)

instance RunMessage SummonedBeast where
  runMessage msg (SummonedBeast attrs) =
    SummonedBeast <$> runMessage msg attrs
