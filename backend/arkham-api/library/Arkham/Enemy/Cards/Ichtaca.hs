module Arkham.Enemy.Cards.Ichtaca (ichtaca, Ichtaca (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted

newtype Ichtaca = Ichtaca EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtaca :: EnemyCard Ichtaca
ichtaca = enemy Ichtaca Cards.ichtaca (5, Static 4, 4) (2, 0)

instance HasAbilities Ichtaca where
  getAbilities (Ichtaca a) =
    extend a [skillTestAbility $ restrictedAbility a 1 OnSameLocation $ parleyAction_]

instance RunMessage Ichtaca where
  runMessage msg e@(Ichtaca attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when attrs.ready $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> Ichtaca <$> liftRunMessage msg attrs
