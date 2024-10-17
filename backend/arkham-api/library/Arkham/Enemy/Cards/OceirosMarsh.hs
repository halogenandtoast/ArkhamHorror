module Arkham.Enemy.Cards.OceirosMarsh (oceirosMarsh, OceirosMarsh (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype OceirosMarsh = OceirosMarsh EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oceirosMarsh :: EnemyCard OceirosMarsh
oceirosMarsh = enemy OceirosMarsh Cards.oceirosMarsh (4, Static 6, 2) (1, 1)

instance RunMessage OceirosMarsh where
  runMessage msg (OceirosMarsh attrs) = runQueueT $ case msg of
    _ -> OceirosMarsh <$> liftRunMessage msg attrs
