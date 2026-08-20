module Arkham.Enemy.Cards.NightOfTheZealot.TheGathering.FleshEater (fleshEater) where

import Arkham.Enemy.CardDefs.NightOfTheZealot.TheGathering qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FleshEater = FleshEater EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fleshEater :: EnemyCard FleshEater
fleshEater = enemyWith FleshEater Cards.fleshEater (spawnAtL ?~ "Attic")

instance RunMessage FleshEater where
  runMessage msg (FleshEater attrs) = FleshEater <$> runMessage msg attrs
