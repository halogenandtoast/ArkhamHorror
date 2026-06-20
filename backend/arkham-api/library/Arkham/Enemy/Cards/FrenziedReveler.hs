module Arkham.Enemy.Cards.FrenziedReveler (frenziedReveler) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher

newtype FrenziedReveler = FrenziedReveler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenziedReveler :: EnemyCard FrenziedReveler
frenziedReveler = enemy FrenziedReveler Cards.frenziedReveler

instance HasAbilities FrenziedReveler where
  getAbilities (FrenziedReveler a) =
    extend
      a
      [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage FrenziedReveler where
  runMessage msg e@(FrenziedReveler attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 4)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      removeFromGame attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeDoomOnAgendaAndCheckAdvance 1
      pure e
    _ -> FrenziedReveler <$> liftRunMessage msg attrs
