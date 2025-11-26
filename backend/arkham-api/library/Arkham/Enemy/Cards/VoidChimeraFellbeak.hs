module Arkham.Enemy.Cards.VoidChimeraFellbeak (voidChimeraFellbeak) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VoidChimeraFellbeak = VoidChimeraFellbeak EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraFellbeak :: EnemyCard VoidChimeraFellbeak
voidChimeraFellbeak = enemy VoidChimeraFellbeak Cards.voidChimeraFellbeak (0, Static 1, 0) (0, 0)

instance RunMessage VoidChimeraFellbeak where
  runMessage msg (VoidChimeraFellbeak attrs) = runQueueT $ case msg of
    _ -> VoidChimeraFellbeak <$> liftRunMessage msg attrs
