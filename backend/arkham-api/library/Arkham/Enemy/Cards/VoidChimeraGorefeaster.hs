module Arkham.Enemy.Cards.VoidChimeraGorefeaster (voidChimeraGorefeaster) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VoidChimeraGorefeaster = VoidChimeraGorefeaster EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraGorefeaster :: EnemyCard VoidChimeraGorefeaster
voidChimeraGorefeaster = enemy VoidChimeraGorefeaster Cards.voidChimeraGorefeaster (0, Static 1, 0) (0, 0)

instance RunMessage VoidChimeraGorefeaster where
  runMessage msg (VoidChimeraGorefeaster attrs) = runQueueT $ case msg of
    _ -> VoidChimeraGorefeaster <$> liftRunMessage msg attrs
