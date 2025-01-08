module Arkham.Enemy.Cards.FleshEater (fleshEater) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FleshEater = FleshEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fleshEater :: EnemyCard FleshEater
fleshEater = enemyWith FleshEater Cards.fleshEater (4, Static 4, 1) (1, 2) (spawnAtL ?~ "Attic")

instance RunMessage FleshEater where
  runMessage msg (FleshEater attrs) = FleshEater <$> runMessage msg attrs
