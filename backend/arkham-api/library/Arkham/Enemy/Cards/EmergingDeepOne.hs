module Arkham.Enemy.Cards.EmergingDeepOne
  ( emergingDeepOne
  , EmergingDeepOne(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype EmergingDeepOne = EmergingDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

emergingDeepOne :: EnemyCard EmergingDeepOne
emergingDeepOne = enemy EmergingDeepOne Cards.emergingDeepOne (3, Static 2, 1) (1, 1)

instance RunMessage EmergingDeepOne where
  runMessage msg (EmergingDeepOne attrs) = runQueueT $ case msg of
    _ -> EmergingDeepOne <$> liftRunMessage msg attrs
