module Arkham.Enemy.Cards.DesiderioDelgadoAlvarez106 (desiderioDelgadoAlvarez106) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DesiderioDelgadoAlvarez106 = DesiderioDelgadoAlvarez106 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

desiderioDelgadoAlvarez106 :: EnemyCard DesiderioDelgadoAlvarez106
desiderioDelgadoAlvarez106 = enemy DesiderioDelgadoAlvarez106 Cards.desiderioDelgadoAlvarez106 (0, Static 1, 0) (0, 0)

instance RunMessage DesiderioDelgadoAlvarez106 where
  runMessage msg (DesiderioDelgadoAlvarez106 attrs) = runQueueT $ case msg of
    _ -> DesiderioDelgadoAlvarez106 <$> liftRunMessage msg attrs
