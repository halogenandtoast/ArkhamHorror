module Arkham.Enemy.Cards.CourtKeeperWeaverOfNightmares (courtKeeperWeaverOfNightmares) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CourtKeeperWeaverOfNightmares = CourtKeeperWeaverOfNightmares EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtKeeperWeaverOfNightmares :: EnemyCard CourtKeeperWeaverOfNightmares
courtKeeperWeaverOfNightmares = enemy CourtKeeperWeaverOfNightmares Cards.courtKeeperWeaverOfNightmares (3, Static 4, 3) (1, 1)

-- TODO: abilities
instance RunMessage CourtKeeperWeaverOfNightmares where
  runMessage msg (CourtKeeperWeaverOfNightmares attrs) = runQueueT $ CourtKeeperWeaverOfNightmares <$> liftRunMessage msg attrs
