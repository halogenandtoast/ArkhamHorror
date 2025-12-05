module Arkham.Enemy.Cards.SlainForemanFamilialPain (slainForemanFamilialPain) where

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

newtype SlainForemanFamilialPain = SlainForemanFamilialPain EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slainForemanFamilialPain :: EnemyCard SlainForemanFamilialPain
slainForemanFamilialPain = enemy SlainForemanFamilialPain Cards.slainForemanFamilialPain (4, Static 5, 2) (1, 1)

instance HasModifiersFor SlainForemanFamilialPain where
  getModifiersFor (SlainForemanFamilialPain a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities SlainForemanFamilialPain where
  getAbilities (SlainForemanFamilialPain a) =
    extend
      a
      [ skillTestAbility
          $ restricted a 1 OnSameLocation
          $ parleyAction (DiscardAssetCost (AssetControlledBy You))
      , mkAbility a 2 $ forced $ EnemyWouldBeDefeated #when (be a)
      ]

instance RunMessage SlainForemanFamilialPain where
  runMessage msg e@(SlainForemanFamilialPain attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let familialPain = lookupCard Stories.familialPain (toCardId attrs)
      focusCards [familialPain] $ continue_ iid
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ (discardPayments -> ps) -> do
      let n = case ps of
            ((_, card) : _) -> printedCardCost card `div` 2
            _ -> 0
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #combat (Fixed $ max 0 $ 6 - n)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _source (isTarget attrs -> True) -> do
      readStoryWithPlacement iid attrs Stories.familialPain (enemyPlacement attrs)
      pure e
    _ -> SlainForemanFamilialPain <$> liftRunMessage msg attrs
