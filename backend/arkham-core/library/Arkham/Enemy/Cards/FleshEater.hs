module Arkham.Enemy.Cards.FleshEater (
  fleshEater,
  FleshEater (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype FleshEater = FleshEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

fleshEater :: EnemyCard FleshEater
fleshEater = enemyWith FleshEater Cards.fleshEater (4, Static 4, 1) (1, 2) (spawnAtL ?~ "Attic")

instance RunMessage FleshEater where
  runMessage msg (FleshEater attrs) = FleshEater <$> runMessage msg attrs
