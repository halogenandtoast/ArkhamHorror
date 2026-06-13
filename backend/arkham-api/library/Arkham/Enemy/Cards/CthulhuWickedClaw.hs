module Arkham.Enemy.Cards.CthulhuWickedClaw (cthulhuWickedClaw) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuWickedClaw = CthulhuWickedClaw EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuWickedClaw :: EnemyCard CthulhuWickedClaw
cthulhuWickedClaw = enemy CthulhuWickedClaw Cards.cthulhuWickedClaw (0, Static 1, 0) (1, 0)

-- TODO: abilities
instance RunMessage CthulhuWickedClaw where
  runMessage msg (CthulhuWickedClaw attrs) = runQueueT $ CthulhuWickedClaw <$> liftRunMessage msg attrs
