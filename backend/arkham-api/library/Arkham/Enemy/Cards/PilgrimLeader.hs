module Arkham.Enemy.Cards.PilgrimLeader (pilgrimLeader) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PilgrimLeader = PilgrimLeader EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pilgrimLeader :: EnemyCard PilgrimLeader
pilgrimLeader = enemy PilgrimLeader Cards.pilgrimLeader (3, Static 3, 3) (1, 1)

-- TODO: abilities
instance RunMessage PilgrimLeader where
  runMessage msg (PilgrimLeader attrs) = runQueueT $ PilgrimLeader <$> liftRunMessage msg attrs
