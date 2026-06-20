module Arkham.Enemy.Cards.IcyGhoul (icyGhoul) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype IcyGhoul = IcyGhoul EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

icyGhoul :: EnemyCard IcyGhoul
icyGhoul = enemyWith IcyGhoul Cards.icyGhoul (spawnAtL ?~ "Cellar")

instance RunMessage IcyGhoul where
  runMessage msg (IcyGhoul attrs) = IcyGhoul <$> runMessage msg attrs
