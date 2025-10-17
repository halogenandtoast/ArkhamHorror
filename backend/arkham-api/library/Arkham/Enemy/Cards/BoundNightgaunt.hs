module Arkham.Enemy.Cards.BoundNightgaunt (boundNightgaunt) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BoundNightgaunt = BoundNightgaunt EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

boundNightgaunt :: EnemyCard BoundNightgaunt
boundNightgaunt = enemy BoundNightgaunt Cards.boundNightgaunt (1, Static 3, 3) (0, 2)

instance RunMessage BoundNightgaunt where
  runMessage msg (BoundNightgaunt attrs) = runQueueT $ case msg of
    _ -> BoundNightgaunt <$> liftRunMessage msg attrs
