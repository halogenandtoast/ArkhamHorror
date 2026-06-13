module Arkham.Enemy.Cards.HuntingParasite (huntingParasite) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HuntingParasite = HuntingParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingParasite :: EnemyCard HuntingParasite
huntingParasite = enemy HuntingParasite Cards.huntingParasite (2, Static 1, 2) (1, 0)

-- TODO: abilities
instance RunMessage HuntingParasite where
  runMessage msg (HuntingParasite attrs) = runQueueT $ HuntingParasite <$> liftRunMessage msg attrs
