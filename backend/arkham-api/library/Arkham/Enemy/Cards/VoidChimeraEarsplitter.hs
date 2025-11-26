module Arkham.Enemy.Cards.VoidChimeraEarsplitter (voidChimeraEarsplitter) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VoidChimeraEarsplitter = VoidChimeraEarsplitter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraEarsplitter :: EnemyCard VoidChimeraEarsplitter
voidChimeraEarsplitter = enemy VoidChimeraEarsplitter Cards.voidChimeraEarsplitter (0, Static 1, 0) (0, 0)

instance RunMessage VoidChimeraEarsplitter where
  runMessage msg (VoidChimeraEarsplitter attrs) = runQueueT $ case msg of
    _ -> VoidChimeraEarsplitter <$> liftRunMessage msg attrs
