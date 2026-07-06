module Arkham.Enemy.Cards.PilgrimAcolyte (pilgrimAcolyte) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PilgrimAcolyte = PilgrimAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pilgrimAcolyte :: EnemyCard PilgrimAcolyte
pilgrimAcolyte = enemy PilgrimAcolyte Cards.pilgrimAcolyte

-- TODO: abilities
instance RunMessage PilgrimAcolyte where
  runMessage msg (PilgrimAcolyte attrs) = runQueueT $ PilgrimAcolyte <$> liftRunMessage msg attrs
