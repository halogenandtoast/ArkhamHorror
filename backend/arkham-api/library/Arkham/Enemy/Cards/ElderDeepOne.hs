module Arkham.Enemy.Cards.ElderDeepOne (elderDeepOne) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ElderDeepOne = ElderDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderDeepOne :: EnemyCard ElderDeepOne
elderDeepOne = enemy ElderDeepOne Cards.elderDeepOne (3, Static 4, 4) (2, 0)

-- TODO: abilities
instance RunMessage ElderDeepOne where
  runMessage msg (ElderDeepOne attrs) = runQueueT $ ElderDeepOne <$> liftRunMessage msg attrs
