module Arkham.Homebrew.CircusExMortis.Enemies.ToweringDarkYoung_065 (toweringDarkYoung_065) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype ToweringDarkYoung_065 = ToweringDarkYoung_065 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringDarkYoung_065 :: EnemyCard ToweringDarkYoung_065
toweringDarkYoung_065 = enemy ToweringDarkYoung_065 Cards.toweringDarkYoung_065

instance HasModifiersFor ToweringDarkYoung_065 where
  getModifiersFor (ToweringDarkYoung_065 a) = modifySelf a [AddKeyword Keyword.Massive]

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
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 0)
      pure e
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      enemyAttackModifier (attrs.ability 1) attrs (DamageDealt (-n))
      pure e
    _ -> ToweringDarkYoung_065 <$> liftRunMessage msg attrs
