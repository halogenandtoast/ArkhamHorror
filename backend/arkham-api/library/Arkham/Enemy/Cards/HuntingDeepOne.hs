module Arkham.Enemy.Cards.HuntingDeepOne
  ( huntingDeepOne
  , HuntingDeepOne(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HuntingDeepOne = HuntingDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

huntingDeepOne :: EnemyCard HuntingDeepOne
huntingDeepOne = enemy HuntingDeepOne Cards.huntingDeepOne (3, Static 3, 3) (1, 1)

instance RunMessage HuntingDeepOne where
  runMessage msg (HuntingDeepOne attrs) = runQueueT $ case msg of
    _ -> HuntingDeepOne <$> liftRunMessage msg attrs
