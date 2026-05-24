module Arkham.Enemy.Cards.CrustaceanHybridInTheLight (crustaceanHybridInTheLight) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CrustaceanHybridInTheLight = CrustaceanHybridInTheLight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

crustaceanHybridInTheLight :: EnemyCard CrustaceanHybridInTheLight
crustaceanHybridInTheLight = enemy CrustaceanHybridInTheLight Cards.crustaceanHybridInTheLight (3, Static 3, 4) (1, 1)

instance RunMessage CrustaceanHybridInTheLight where
  runMessage msg (CrustaceanHybridInTheLight attrs) = runQueueT $ case msg of
    _ -> CrustaceanHybridInTheLight <$> liftRunMessage msg attrs
