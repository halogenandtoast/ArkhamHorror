module Arkham.Enemy.Cards.BuriedMinerExhumeTheBones (buriedMinerExhumeTheBones) where

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

newtype BuriedMinerExhumeTheBones = BuriedMinerExhumeTheBones EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buriedMinerExhumeTheBones :: EnemyCard BuriedMinerExhumeTheBones
buriedMinerExhumeTheBones = enemy BuriedMinerExhumeTheBones Cards.buriedMinerExhumeTheBones (2, Static 4, 4) (1, 1)

instance HasModifiersFor BuriedMinerExhumeTheBones where
  getModifiersFor (BuriedMinerExhumeTheBones a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities BuriedMinerExhumeTheBones where
  getAbilities (BuriedMinerExhumeTheBones a) =
    extend
      a
      [ skillTestAbility
          $ restricted a 1 OnSameLocation
          $ parleyAction (AtLeastOne (Fixed 3) (HorrorCost (a.ability 1) YouTarget 1))
      , mkAbility a 2 $ forced $ EnemyWouldBeDefeated #when (be a)
      ]

instance RunMessage BuriedMinerExhumeTheBones where
  runMessage msg e@(BuriedMinerExhumeTheBones attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let exhumeTheBones = lookupCard Stories.exhumeTheBones (toCardId attrs)
      focusCards [exhumeTheBones] $ continue_ iid
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ (horrorPaid -> n) -> do
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
      readStoryWithPlacement iid attrs Stories.exhumeTheBones (enemyPlacement attrs)
      pure e
    _ -> BuriedMinerExhumeTheBones <$> liftRunMessage msg attrs
