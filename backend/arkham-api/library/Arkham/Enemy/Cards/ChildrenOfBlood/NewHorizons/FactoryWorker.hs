module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.FactoryWorker (factoryWorker) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype FactoryWorker = FactoryWorker EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryWorker :: EnemyCard FactoryWorker
factoryWorker = enemy FactoryWorker Cards.factoryWorker

instance HasModifiersFor FactoryWorker where
  getModifiersFor (FactoryWorker a) = modifySelect a (locationWithEnemy a.id) [ShroudModifier 1]

instance HasAbilities FactoryWorker where
  getAbilities (FactoryWorker a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage FactoryWorker where
  runMessage msg e@(FactoryWorker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> FactoryWorker <$> liftRunMessage msg attrs
