module Arkham.Enemy.Cards.JosefMeiger (josefMeiger, JosefMeiger (..)) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story
import Arkham.Trait (Trait (SilverTwilight))

newtype JosefMeiger = JosefMeiger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

josefMeiger :: EnemyCard JosefMeiger
josefMeiger = enemy JosefMeiger Cards.josefMeiger (3, Static 3, 3) (1, 1)

instance HasAbilities JosefMeiger where
  getAbilities (JosefMeiger a) =
    withBaseAbilities
      a
      [ restrictedAbility
          a
          1
          (OnSameLocation <> notExists (withTrait SilverTwilight <> EnemyWithAnyDoom <> not_ (be a)))
          parleyAction_
      ]

instance RunMessage JosefMeiger where
  runMessage msg e@(JosefMeiger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      josefsPlan <- genCard Story.josefsPlan
      push $ ReadStory iid josefsPlan ResolveIt (Just $ toTarget attrs)
      pure e
    _ -> JosefMeiger <$> liftRunMessage msg attrs
