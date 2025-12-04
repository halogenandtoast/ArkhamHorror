module Arkham.Enemy.Cards.UncannyShadowTimorousShadows (uncannyShadowTimorousShadows) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype UncannyShadowTimorousShadows = UncannyShadowTimorousShadows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

uncannyShadowTimorousShadows :: EnemyCard UncannyShadowTimorousShadows
uncannyShadowTimorousShadows = enemy UncannyShadowTimorousShadows Cards.uncannyShadowTimorousShadows (0, Static 1, 0) (0, 0)

instance RunMessage UncannyShadowTimorousShadows where
  runMessage msg (UncannyShadowTimorousShadows attrs) = runQueueT $ case msg of
    _ -> UncannyShadowTimorousShadows <$> liftRunMessage msg attrs
