module Arkham.Enemy.Cards.LeahAtwood (leahAtwood) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Message.Lifted.Placement

newtype LeahAtwood = LeahAtwood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leahAtwood :: EnemyCard LeahAtwood
leahAtwood = enemy LeahAtwood Cards.leahAtwood

instance HasAbilities LeahAtwood where
  getAbilities (LeahAtwood a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage LeahAtwood where
  runMessage msg e@(LeahAtwood attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #combat (Fixed 5)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> LeahAtwood <$> liftRunMessage msg attrs
