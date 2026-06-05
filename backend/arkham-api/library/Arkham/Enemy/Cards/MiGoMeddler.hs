module Arkham.Enemy.Cards.MiGoMeddler (miGoMeddler) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MiGoMeddler = MiGoMeddler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

miGoMeddler :: EnemyCard MiGoMeddler
miGoMeddler = enemy MiGoMeddler Cards.miGoMeddler (3, PerPlayer 2, 4) (1, 0)

instance RunMessage MiGoMeddler where
  runMessage msg (MiGoMeddler attrs) = MiGoMeddler <$> runMessage msg attrs
