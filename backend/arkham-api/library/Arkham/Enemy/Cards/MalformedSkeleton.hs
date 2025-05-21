module Arkham.Enemy.Cards.MalformedSkeleton (malformedSkeleton) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MalformedSkeleton = MalformedSkeleton EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

malformedSkeleton :: EnemyCard MalformedSkeleton
malformedSkeleton = enemy MalformedSkeleton Cards.malformedSkeleton (0, Static 1, 0) (0, 0)

instance RunMessage MalformedSkeleton where
  runMessage msg (MalformedSkeleton attrs) = runQueueT $ case msg of
    _ -> MalformedSkeleton <$> liftRunMessage msg attrs
