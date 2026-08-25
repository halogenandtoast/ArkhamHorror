module Arkham.Homebrew.CircusExMortis.Enemies.ToweringDarkYoung_066 (toweringDarkYoung_066) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype ToweringDarkYoung_066 = ToweringDarkYoung_066 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringDarkYoung_066 :: EnemyCard ToweringDarkYoung_066
toweringDarkYoung_066 = enemy ToweringDarkYoung_066 Cards.toweringDarkYoung_066

instance HasModifiersFor ToweringDarkYoung_066 where
  getModifiersFor (ToweringDarkYoung_066 a) = modifySelf a [AddKeyword Keyword.Massive]

instance HasAbilities ToweringDarkYoung_066 where
  getAbilities (ToweringDarkYoung_066 a) =
    extend1 a
      $ skillTestAbility
      $ mkAbility a 1
      $ freeReaction
      $ EnemyWouldAttack #when You AnyEnemyAttack (be a)

instance RunMessage ToweringDarkYoung_066 where
  runMessage msg e@(ToweringDarkYoung_066 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 0)
      pure e
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      enemyAttackModifier (attrs.ability 1) attrs (HorrorDealt (-n))
      pure e
    _ -> ToweringDarkYoung_066 <$> liftRunMessage msg attrs
