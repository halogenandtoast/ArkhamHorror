module Arkham.Enemy.Cards.CthulhuAncientEvil (cthulhuAncientEvil) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuAncientEvil = CthulhuAncientEvil EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuAncientEvil :: EnemyCard CthulhuAncientEvil
cthulhuAncientEvil = enemy CthulhuAncientEvil Cards.cthulhuAncientEvil (0, Static 1, 0) (0, 0)

-- TODO: abilities
instance RunMessage CthulhuAncientEvil where
  runMessage msg (CthulhuAncientEvil attrs) = runQueueT $ CthulhuAncientEvil <$> liftRunMessage msg attrs
