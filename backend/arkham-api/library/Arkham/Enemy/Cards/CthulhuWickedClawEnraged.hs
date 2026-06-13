module Arkham.Enemy.Cards.CthulhuWickedClawEnraged (cthulhuWickedClawEnraged) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuWickedClawEnraged = CthulhuWickedClawEnraged EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuWickedClawEnraged :: EnemyCard CthulhuWickedClawEnraged
cthulhuWickedClawEnraged = enemy CthulhuWickedClawEnraged Cards.cthulhuWickedClawEnraged (0, Static 1, 0) (1, 0)

-- TODO: abilities
instance RunMessage CthulhuWickedClawEnraged where
  runMessage msg (CthulhuWickedClawEnraged attrs) = runQueueT $ CthulhuWickedClawEnraged <$> liftRunMessage msg attrs
