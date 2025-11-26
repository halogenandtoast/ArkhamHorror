module Arkham.Enemy.Cards.RavenousGrizzly (ravenousGrizzly) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype RavenousGrizzly = RavenousGrizzly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ravenousGrizzly :: EnemyCard RavenousGrizzly
ravenousGrizzly = enemy RavenousGrizzly Cards.ravenousGrizzly (0, Static 1, 0) (0, 0)

instance RunMessage RavenousGrizzly where
  runMessage msg (RavenousGrizzly attrs) = runQueueT $ case msg of
    _ -> RavenousGrizzly <$> liftRunMessage msg attrs
