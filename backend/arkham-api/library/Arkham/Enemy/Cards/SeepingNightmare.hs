module Arkham.Enemy.Cards.SeepingNightmare (seepingNightmare)  where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SeepingNightmare = SeepingNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

seepingNightmare :: EnemyCard SeepingNightmare
seepingNightmare = enemy SeepingNightmare Cards.seepingNightmare (3, Static 6, 3) (2, 2)

instance RunMessage SeepingNightmare where
  runMessage msg (SeepingNightmare attrs) = runQueueT $ case msg of
    _ -> SeepingNightmare <$> liftRunMessage msg attrs
