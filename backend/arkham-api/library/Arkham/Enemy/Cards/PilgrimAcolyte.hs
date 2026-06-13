module Arkham.Enemy.Cards.PilgrimAcolyte (pilgrimAcolyte) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PilgrimAcolyte = PilgrimAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pilgrimAcolyte :: EnemyCard PilgrimAcolyte
pilgrimAcolyte = enemy PilgrimAcolyte Cards.pilgrimAcolyte (2, Static 2, 2) (1, 0)

-- TODO: abilities
instance RunMessage PilgrimAcolyte where
  runMessage msg (PilgrimAcolyte attrs) = runQueueT $ PilgrimAcolyte <$> liftRunMessage msg attrs
