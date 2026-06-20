module Arkham.Enemy.Cards.StowawayDrone (stowawayDrone) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype StowawayDrone = StowawayDrone EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stowawayDrone :: EnemyCard StowawayDrone
stowawayDrone = enemy StowawayDrone Cards.stowawayDrone

-- TODO: abilities
instance RunMessage StowawayDrone where
  runMessage msg (StowawayDrone attrs) = runQueueT $ StowawayDrone <$> liftRunMessage msg attrs
