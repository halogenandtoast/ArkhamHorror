module Arkham.Enemy.Cards.DimensionalShambler
  ( dimensionalShambler
  , DimensionalShambler(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype DimensionalShambler = DimensionalShambler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalShambler :: EnemyCard DimensionalShambler
dimensionalShambler = enemy DimensionalShambler Cards.dimensionalShambler (4, PerPlayer 4, 3) (2, 2)

instance RunMessage DimensionalShambler where
  runMessage msg (DimensionalShambler attrs) =
    DimensionalShambler <$> runMessage msg attrs
