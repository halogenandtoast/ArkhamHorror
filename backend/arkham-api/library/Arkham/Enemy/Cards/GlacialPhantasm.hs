module Arkham.Enemy.Cards.GlacialPhantasm ( glacialPhantasm , GlacialPhantasm(..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GlacialPhantasm = GlacialPhantasm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

glacialPhantasm :: EnemyCard GlacialPhantasm
glacialPhantasm = enemy GlacialPhantasm Cards.glacialPhantasm (0, Static 1, 0) (0, 0)

instance RunMessage GlacialPhantasm where
  runMessage msg (GlacialPhantasm attrs) = runQueueT $ case msg of
    _ -> GlacialPhantasm <$> liftRunMessage msg attrs
