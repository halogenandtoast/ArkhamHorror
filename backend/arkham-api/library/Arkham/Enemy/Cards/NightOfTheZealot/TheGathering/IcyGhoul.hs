module Arkham.Enemy.Cards.NightOfTheZealot.TheGathering.IcyGhoul (icyGhoul) where

import Arkham.Enemy.CardDefs.NightOfTheZealot.TheGathering qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype IcyGhoul = IcyGhoul EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

icyGhoul :: EnemyCard IcyGhoul
icyGhoul = enemyWith IcyGhoul Cards.icyGhoul (spawnAtL ?~ "Cellar")

instance RunMessage IcyGhoul where
  runMessage msg (IcyGhoul attrs) = IcyGhoul <$> runMessage msg attrs
