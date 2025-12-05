module Arkham.Enemy.Cards.UncannyShadowPlayfulShadows (uncannyShadowPlayfulShadows) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Story.Cards qualified as Stories

newtype UncannyShadowPlayfulShadows = UncannyShadowPlayfulShadows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

uncannyShadowPlayfulShadows :: EnemyCard UncannyShadowPlayfulShadows
uncannyShadowPlayfulShadows = enemy UncannyShadowPlayfulShadows Cards.uncannyShadowPlayfulShadows (0, Static 1, 0) (0, 0)

instance RunMessage UncannyShadowPlayfulShadows where
  runMessage msg e@(UncannyShadowPlayfulShadows attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let playfulShadows = lookupCard Stories.playfulShadows (toCardId attrs)
      focusCards [playfulShadows] $ continue_ iid
      pure e
    _ -> UncannyShadowPlayfulShadows <$> liftRunMessage msg attrs
