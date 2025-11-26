module Arkham.Enemy.Cards.VoidChimeraFellhound (voidChimeraFellhound) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VoidChimeraFellhound = VoidChimeraFellhound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraFellhound :: EnemyCard VoidChimeraFellhound
voidChimeraFellhound = enemy VoidChimeraFellhound Cards.voidChimeraFellhound (0, Static 1, 0) (0, 0)

instance RunMessage VoidChimeraFellhound where
  runMessage msg (VoidChimeraFellhound attrs) = runQueueT $ case msg of
    _ -> VoidChimeraFellhound <$> liftRunMessage msg attrs
