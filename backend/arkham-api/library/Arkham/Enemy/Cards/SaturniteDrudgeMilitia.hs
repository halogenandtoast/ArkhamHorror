module Arkham.Enemy.Cards.SaturniteDrudgeMilitia (saturniteDrudgeMilitia) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SaturniteDrudgeMilitia = SaturniteDrudgeMilitia EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saturniteDrudgeMilitia :: EnemyCard SaturniteDrudgeMilitia
saturniteDrudgeMilitia = enemy SaturniteDrudgeMilitia Cards.saturniteDrudgeMilitia (3, Static 5, 3) (1, 1)

instance HasAbilities SaturniteDrudgeMilitia where
  getAbilities (SaturniteDrudgeMilitia a) = extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage SaturniteDrudgeMilitia where
  runMessage msg e@(SaturniteDrudgeMilitia attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure e
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) n attrs
      when (n >= 2) $ exhaustThis attrs
      pure e
    _ -> SaturniteDrudgeMilitia <$> liftRunMessage msg attrs
