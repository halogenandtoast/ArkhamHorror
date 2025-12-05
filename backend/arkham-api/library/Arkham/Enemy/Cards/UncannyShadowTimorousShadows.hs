module Arkham.Enemy.Cards.UncannyShadowTimorousShadows (uncannyShadowTimorousShadows) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Story.Cards qualified as Stories

newtype UncannyShadowTimorousShadows = UncannyShadowTimorousShadows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

uncannyShadowTimorousShadows :: EnemyCard UncannyShadowTimorousShadows
uncannyShadowTimorousShadows = enemy UncannyShadowTimorousShadows Cards.uncannyShadowTimorousShadows (0, Static 1, 0) (0, 0)

instance RunMessage UncannyShadowTimorousShadows where
  runMessage msg e@(UncannyShadowTimorousShadows attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let timorousShadows = lookupCard Stories.timorousShadows (toCardId attrs)
      focusCards [timorousShadows] $ continue_ iid
      pure e
    _ -> UncannyShadowTimorousShadows <$> liftRunMessage msg attrs
