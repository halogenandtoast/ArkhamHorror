module Arkham.Enemy.Cards.CerenerianDeepOne
  ( cerenerianDeepOne
  , CerenerianDeepOne(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CerenerianDeepOne = CerenerianDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cerenerianDeepOne :: EnemyCard CerenerianDeepOne
cerenerianDeepOne = enemy CerenerianDeepOne Cards.cerenerianDeepOne (2, Static 3, 2) (1, 1)

instance RunMessage CerenerianDeepOne where
  runMessage msg (CerenerianDeepOne attrs) = runQueueT $ case msg of
    _ -> CerenerianDeepOne <$> liftRunMessage msg attrs
