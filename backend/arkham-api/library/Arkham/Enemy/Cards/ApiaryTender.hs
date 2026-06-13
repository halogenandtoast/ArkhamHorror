module Arkham.Enemy.Cards.ApiaryTender (apiaryTender) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ApiaryTender = ApiaryTender EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apiaryTender :: EnemyCard ApiaryTender
apiaryTender = enemy ApiaryTender Cards.apiaryTender (3, Static 3, 3) (1, 1)

-- TODO: abilities
instance RunMessage ApiaryTender where
  runMessage msg (ApiaryTender attrs) = runQueueT $ ApiaryTender <$> liftRunMessage msg attrs
