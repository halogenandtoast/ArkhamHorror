module Arkham.Enemy.Cards.SlainForemanSympathyPain (slainForemanSympathyPain) where

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

newtype SlainForemanSympathyPain = SlainForemanSympathyPain EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slainForemanSympathyPain :: EnemyCard SlainForemanSympathyPain
slainForemanSympathyPain = enemy SlainForemanSympathyPain Cards.slainForemanSympathyPain (4, Static 5, 2) (1, 1)

instance HasModifiersFor SlainForemanSympathyPain where
  getModifiersFor (SlainForemanSympathyPain a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities SlainForemanSympathyPain where
  getAbilities (SlainForemanSympathyPain a) =
    extend
      a
      [ skillTestAbility
          $ restricted a 1 OnSameLocation
          $ parleyAction (DiscardAssetCost (AssetControlledBy You))
      , mkAbility a 2 $ forced $ EnemyWouldBeDefeated #when (be a)
      ]

instance RunMessage SlainForemanSympathyPain where
  runMessage msg e@(SlainForemanSympathyPain attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let sympathyPain = lookupCard Stories.sympathyPain (toCardId attrs)
      focusCards [sympathyPain] $ continue_ iid
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ (discardPayments -> ps) -> do
      let n = case ps of
            ((_, card) : _) -> printedCardCost card `div` 2
            _ -> 0
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed $ max 0 $ 6 - n)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _source (isTarget attrs -> True) -> do
      readStoryWithPlacement iid attrs Stories.sympathyPain (enemyPlacement attrs)
      pure e
    _ -> SlainForemanSympathyPain <$> liftRunMessage msg attrs
