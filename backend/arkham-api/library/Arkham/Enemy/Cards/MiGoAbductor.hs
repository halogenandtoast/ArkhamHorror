module Arkham.Enemy.Cards.MiGoAbductor (miGoAbductor) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MiGoAbductor = MiGoAbductor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

miGoAbductor :: EnemyCard MiGoAbductor
miGoAbductor = enemy MiGoAbductor Cards.miGoAbductor (4, PerPlayer 2, 2) (1, 1)

instance RunMessage MiGoAbductor where
  runMessage msg (MiGoAbductor attrs) = MiGoAbductor <$> runMessage msg attrs
