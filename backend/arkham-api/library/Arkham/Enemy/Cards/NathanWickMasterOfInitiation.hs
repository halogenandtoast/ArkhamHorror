module Arkham.Enemy.Cards.NathanWickMasterOfInitiation (nathanWickMasterOfInitiation) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.SkillTest.Lifted

newtype NathanWickMasterOfInitiation = NathanWickMasterOfInitiation EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanWickMasterOfInitiation :: EnemyCard NathanWickMasterOfInitiation
nathanWickMasterOfInitiation = enemy NathanWickMasterOfInitiation Cards.nathanWickMasterOfInitiation (3, Static 5, 4) (1, 1)

instance HasAbilities NathanWickMasterOfInitiation where
  getAbilities (NathanWickMasterOfInitiation a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage NathanWickMasterOfInitiation where
  runMessage msg e@(NathanWickMasterOfInitiation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      doStep 1 msg
      pure e
    DoStep 1 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      n <- perPlayer 1
      when (attrs.token #resource >= n) do
        for_ attrs.keys (placeKey iid)
        addToVictory attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> NathanWickMasterOfInitiation <$> liftRunMessage msg attrs
