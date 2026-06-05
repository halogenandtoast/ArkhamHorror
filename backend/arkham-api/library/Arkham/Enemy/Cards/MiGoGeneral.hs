module Arkham.Enemy.Cards.MiGoGeneral (miGoGeneral) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MiGoGeneral = MiGoGeneral EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

miGoGeneral :: EnemyCard MiGoGeneral
miGoGeneral = enemy MiGoGeneral Cards.miGoGeneral (5, PerPlayer 4, 2) (1, 1)

instance RunMessage MiGoGeneral where
  runMessage msg (MiGoGeneral attrs) = MiGoGeneral <$> runMessage msg attrs
