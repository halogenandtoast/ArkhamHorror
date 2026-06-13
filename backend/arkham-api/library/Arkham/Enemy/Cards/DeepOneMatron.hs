module Arkham.Enemy.Cards.DeepOneMatron (deepOneMatron) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DeepOneMatron = DeepOneMatron EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneMatron :: EnemyCard DeepOneMatron
deepOneMatron = enemy DeepOneMatron Cards.deepOneMatron (3, Static 6, 3) (0, 1)

-- TODO: abilities
instance RunMessage DeepOneMatron where
  runMessage msg (DeepOneMatron attrs) = runQueueT $ DeepOneMatron <$> liftRunMessage msg attrs
