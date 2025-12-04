module Arkham.Enemy.Cards.SlainForemanFamilialPain (slainForemanFamilialPain) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SlainForemanFamilialPain = SlainForemanFamilialPain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slainForemanFamilialPain :: EnemyCard SlainForemanFamilialPain
slainForemanFamilialPain = enemy SlainForemanFamilialPain Cards.slainForemanFamilialPain (0, Static 1, 0) (0, 0)

instance RunMessage SlainForemanFamilialPain where
  runMessage msg (SlainForemanFamilialPain attrs) = runQueueT $ case msg of
    _ -> SlainForemanFamilialPain <$> liftRunMessage msg attrs
