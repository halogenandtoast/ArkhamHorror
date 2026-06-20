module Arkham.Enemy.Cards.WilliamHemlock (williamHemlock) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Message.Lifted.Placement

newtype WilliamHemlock = WilliamHemlock EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamHemlock :: EnemyCard WilliamHemlock
williamHemlock = enemy WilliamHemlock Cards.williamHemlock

instance HasAbilities WilliamHemlock where
  getAbilities (WilliamHemlock a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage WilliamHemlock where
  runMessage msg e@(WilliamHemlock attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 5)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> WilliamHemlock <$> liftRunMessage msg attrs
