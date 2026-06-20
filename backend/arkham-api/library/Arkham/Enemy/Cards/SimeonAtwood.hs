module Arkham.Enemy.Cards.SimeonAtwood (simeonAtwood) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Message.Lifted.Placement

newtype SimeonAtwood = SimeonAtwood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

simeonAtwood :: EnemyCard SimeonAtwood
simeonAtwood = enemy SimeonAtwood Cards.simeonAtwood

instance HasAbilities SimeonAtwood where
  getAbilities (SimeonAtwood a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage SimeonAtwood where
  runMessage msg e@(SimeonAtwood attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 5)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> SimeonAtwood <$> liftRunMessage msg attrs
