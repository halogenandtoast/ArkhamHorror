module Arkham.Homebrew.CircusExMortis.Enemies.ToweringDarkYoung_065 (toweringDarkYoung_065) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype ToweringDarkYoung_065 = ToweringDarkYoung_065 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringDarkYoung_065 :: EnemyCard ToweringDarkYoung_065
toweringDarkYoung_065 = enemy ToweringDarkYoung_065 Cards.toweringDarkYoung_065

instance HasAbilities ToweringDarkYoung_065 where
  getAbilities (ToweringDarkYoung_065 a) =
    extend1 a
      $ skillTestAbility
      $ mkAbility a 1
      $ freeReaction
      $ EnemyWouldAttack #when You AnyEnemyAttack (be a)

instance RunMessage ToweringDarkYoung_065 where
  runMessage msg e@(ToweringDarkYoung_065 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Keep the attack behind this test, including when it interrupts another test.
      let isAttackWindow = \case
            Window.EnemyAttacksEvenIfCancelled details -> details.enemy == attrs.id
            _ -> False
      moveWithSkillTest \case
        PerformEnemyAttack eid -> eid == attrs.id
        After (PerformEnemyAttack eid) -> eid == attrs.id
        CheckWindows ws -> any (isAttackWindow . Window.windowType) ws
        Do (CheckWindows ws) -> any (isAttackWindow . Window.windowType) ws
        _ -> False
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 0)
      pure e
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      enemyAttackModifier (attrs.ability 1) attrs (DamageDealt (-n))
      pure e
    _ -> ToweringDarkYoung_065 <$> liftRunMessage msg attrs
