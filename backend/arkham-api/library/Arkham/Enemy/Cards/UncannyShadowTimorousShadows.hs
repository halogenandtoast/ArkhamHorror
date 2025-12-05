module Arkham.Enemy.Cards.UncannyShadowTimorousShadows (uncannyShadowTimorousShadows) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype UncannyShadowTimorousShadows = UncannyShadowTimorousShadows EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncannyShadowTimorousShadows :: EnemyCard UncannyShadowTimorousShadows
uncannyShadowTimorousShadows = enemy UncannyShadowTimorousShadows Cards.uncannyShadowTimorousShadows (3, Static 4, 3) (1, 1)

instance HasModifiersFor UncannyShadowTimorousShadows where
  getModifiersFor (UncannyShadowTimorousShadows a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities UncannyShadowTimorousShadows where
  getAbilities (UncannyShadowTimorousShadows a) =
    extend
      a
      [ skillTestAbility
          $ restricted a 1 OnSameLocation
          $ parleyAction (AtLeastOne (Fixed 3) DiscardRandomCardCost)
      , mkAbility a 2 $ forced $ EnemyWouldBeDefeated #when (be a)
      ]

instance RunMessage UncannyShadowTimorousShadows where
  runMessage msg e@(UncannyShadowTimorousShadows attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let timorousShadows = lookupCard Stories.timorousShadows (toCardId attrs)
      focusCards [timorousShadows] $ continue_ iid
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalDiscardCardPayments -> n) -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed $ max 0 $ 6 - n)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _source (isTarget attrs -> True) -> do
      readStoryWithPlacement iid attrs Stories.timorousShadows (enemyPlacement attrs)
      pure e
    _ -> UncannyShadowTimorousShadows <$> liftRunMessage msg attrs
