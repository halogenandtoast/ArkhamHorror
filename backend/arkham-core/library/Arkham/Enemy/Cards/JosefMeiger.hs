module Arkham.Enemy.Cards.JosefMeiger (
  josefMeiger,
  JosefMeiger (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.SkillType
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
          ( OnSameLocation
              <> Negate
                ( EnemyCriteria
                    $ EnemyExists
                    $ EnemyWithTrait SilverTwilight
                    <> EnemyWithAnyDoom
                    <> NotEnemy (EnemyWithId $ toId a)
                )
          )
          $ ActionAbility [Action.Parley]
          $ ActionCost 1
      ]

instance RunMessage JosefMeiger where
  runMessage msg e@(JosefMeiger attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs attrs SkillIntellect 4
      pure e
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
        pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ Flip iid (toSource attrs) (toTarget attrs)
        pure e
    Flip iid _ target | isTarget attrs target -> do
      josefsPlan <- genCard Story.josefsPlan
      push $ ReadStory iid josefsPlan ResolveIt (Just $ toTarget attrs)
      pure e
    _ -> JosefMeiger <$> runMessage msg attrs
