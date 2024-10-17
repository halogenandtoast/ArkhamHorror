module Arkham.Enemy.Cards.DeepOneNursemaid
  ( deepOneNursemaid
  , DeepOneNursemaid(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DeepOneNursemaid = DeepOneNursemaid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

deepOneNursemaid :: EnemyCard DeepOneNursemaid
deepOneNursemaid = enemy DeepOneNursemaid Cards.deepOneNursemaid (0, Static 1, 0) (0, 0)

instance RunMessage DeepOneNursemaid where
  runMessage msg (DeepOneNursemaid attrs) = runQueueT $ case msg of
    _ -> DeepOneNursemaid <$> liftRunMessage msg attrs
