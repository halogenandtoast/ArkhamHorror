module Arkham.Enemy.Cards.MiGoHarvester (miGoHarvester) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MiGoHarvester = MiGoHarvester EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

miGoHarvester :: EnemyCard MiGoHarvester
miGoHarvester = enemy MiGoHarvester Cards.miGoHarvester (2, PerPlayer 2, 4) (1, 0)

instance RunMessage MiGoHarvester where
  runMessage msg (MiGoHarvester attrs) = MiGoHarvester <$> runMessage msg attrs
