module Arkham.Enemy.Cards.PrimordialEvil (primordialEvil) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PrimordialEvil = PrimordialEvil EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

primordialEvil :: EnemyCard PrimordialEvil
primordialEvil = enemy PrimordialEvil Cards.primordialEvil (3, Static 5, 1) (2, 1)

instance RunMessage PrimordialEvil where
  runMessage msg (PrimordialEvil attrs) = runQueueT $ case msg of
    _ -> PrimordialEvil <$> liftRunMessage msg attrs
