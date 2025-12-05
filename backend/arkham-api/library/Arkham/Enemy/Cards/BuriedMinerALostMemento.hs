module Arkham.Enemy.Cards.BuriedMinerALostMemento (buriedMinerALostMemento) where

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

newtype BuriedMinerALostMemento = BuriedMinerALostMemento EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buriedMinerALostMemento :: EnemyCard BuriedMinerALostMemento
buriedMinerALostMemento = enemy BuriedMinerALostMemento Cards.buriedMinerALostMemento (2, Static 4, 4) (1, 1)

instance HasModifiersFor BuriedMinerALostMemento where
  getModifiersFor (BuriedMinerALostMemento a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities BuriedMinerALostMemento where
  getAbilities (BuriedMinerALostMemento a) =
    extend
      a
      [ skillTestAbility
          $ restricted a 1 OnSameLocation
          $ parleyAction (AtLeastOne (Fixed 3) (HorrorCost (a.ability 1) YouTarget 1))
      , mkAbility a 2 $ forced $ EnemyWouldBeDefeated #when (be a)
      ]

instance RunMessage BuriedMinerALostMemento where
  runMessage msg e@(BuriedMinerALostMemento attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let aLostMemento = lookupCard Stories.aLostMemento (toCardId attrs)
      focusCards [aLostMemento] $ continue_ iid
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
      readStoryWithPlacement iid attrs Stories.aLostMemento (enemyPlacement attrs)
      pure e
    _ -> BuriedMinerALostMemento <$> liftRunMessage msg attrs
