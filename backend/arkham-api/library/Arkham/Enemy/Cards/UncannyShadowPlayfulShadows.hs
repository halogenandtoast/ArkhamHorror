module Arkham.Enemy.Cards.UncannyShadowPlayfulShadows (uncannyShadowPlayfulShadows) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype UncannyShadowPlayfulShadows = UncannyShadowPlayfulShadows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

uncannyShadowPlayfulShadows :: EnemyCard UncannyShadowPlayfulShadows
uncannyShadowPlayfulShadows = enemy UncannyShadowPlayfulShadows Cards.uncannyShadowPlayfulShadows (0, Static 1, 0) (0, 0)

instance RunMessage UncannyShadowPlayfulShadows where
  runMessage msg (UncannyShadowPlayfulShadows attrs) = runQueueT $ case msg of
    _ -> UncannyShadowPlayfulShadows <$> liftRunMessage msg attrs
