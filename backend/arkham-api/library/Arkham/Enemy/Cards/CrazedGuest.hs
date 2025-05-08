module Arkham.Enemy.Cards.CrazedGuest (crazedGuest) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CrazedGuest = CrazedGuest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crazedGuest :: EnemyCard CrazedGuest
crazedGuest = enemy CrazedGuest Cards.crazedGuest (3, Static 5, 4) (1, 0)

instance HasAbilities CrazedGuest where
  getAbilities (CrazedGuest a) = extend1 a $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage CrazedGuest where
  runMessage msg e@(CrazedGuest attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      automaticallyEvadeEnemy iid attrs
      pure e
    _ -> CrazedGuest <$> liftRunMessage msg attrs
