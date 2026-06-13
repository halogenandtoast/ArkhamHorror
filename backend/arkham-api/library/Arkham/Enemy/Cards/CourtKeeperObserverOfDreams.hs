module Arkham.Enemy.Cards.CourtKeeperObserverOfDreams (courtKeeperObserverOfDreams) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CourtKeeperObserverOfDreams = CourtKeeperObserverOfDreams EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtKeeperObserverOfDreams :: EnemyCard CourtKeeperObserverOfDreams
courtKeeperObserverOfDreams = enemy CourtKeeperObserverOfDreams Cards.courtKeeperObserverOfDreams (3, Static 4, 3) (1, 1)

-- TODO: abilities
instance RunMessage CourtKeeperObserverOfDreams where
  runMessage msg (CourtKeeperObserverOfDreams attrs) = runQueueT $ CourtKeeperObserverOfDreams <$> liftRunMessage msg attrs
