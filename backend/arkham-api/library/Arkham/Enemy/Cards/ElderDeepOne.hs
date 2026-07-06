module Arkham.Enemy.Cards.ElderDeepOne (elderDeepOne) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ElderDeepOne = ElderDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

elderDeepOne :: EnemyCard ElderDeepOne
elderDeepOne = enemy ElderDeepOne Cards.elderDeepOne

-- TODO: abilities
instance RunMessage ElderDeepOne where
  runMessage msg (ElderDeepOne attrs) = runQueueT $ ElderDeepOne <$> liftRunMessage msg attrs
