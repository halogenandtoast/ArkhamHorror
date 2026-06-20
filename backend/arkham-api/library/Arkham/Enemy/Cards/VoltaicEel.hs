module Arkham.Enemy.Cards.VoltaicEel (voltaicEel) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VoltaicEel = VoltaicEel EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voltaicEel :: EnemyCard VoltaicEel
voltaicEel = enemy VoltaicEel Cards.voltaicEel

-- TODO: abilities
instance RunMessage VoltaicEel where
  runMessage msg (VoltaicEel attrs) = runQueueT $ VoltaicEel <$> liftRunMessage msg attrs
