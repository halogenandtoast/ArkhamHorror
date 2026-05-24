module Arkham.Enemy.Cards.CrustaceanHybridInTheDark (crustaceanHybridInTheDark) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CrustaceanHybridInTheDark = CrustaceanHybridInTheDark EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

crustaceanHybridInTheDark :: EnemyCard CrustaceanHybridInTheDark
crustaceanHybridInTheDark = enemy CrustaceanHybridInTheDark Cards.crustaceanHybridInTheDark (4, Static 3, 3) (1, 1)

instance RunMessage CrustaceanHybridInTheDark where
  runMessage msg (CrustaceanHybridInTheDark attrs) = runQueueT $ case msg of
    _ -> CrustaceanHybridInTheDark <$> liftRunMessage msg attrs
