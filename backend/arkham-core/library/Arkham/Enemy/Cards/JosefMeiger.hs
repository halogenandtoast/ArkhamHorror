module Arkham.Enemy.Cards.JosefMeiger
  ( josefMeiger
  , josefMeigerEffect
  , JosefMeiger(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Story.Cards qualified as Story
import Arkham.Trait (Trait(SilverTwilight))

newtype JosefMeiger = JosefMeiger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

josefMeiger :: EnemyCard JosefMeiger
josefMeiger = enemy JosefMeiger Cards.josefMeiger (3, Static 3, 3) (1, 1)

instance HasAbilities JosefMeiger where
  getAbilities (JosefMeiger a) = withBaseAbilities a
    [ restrictedAbility a 1
        (OnSameLocation <> Negate (EnemyCriteria $ EnemyExists $ EnemyWithTrait SilverTwilight <> EnemyWithAnyDoom <> NotEnemy (EnemyWithId $ toId a)))
      $ ActionAbility (Just Action.Parley)
      $ ActionCost 1
    ]

instance RunMessage JosefMeiger where
  runMessage msg e@(JosefMeiger attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs attrs SkillIntellect 4
      pure e
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ InitiateEnemyAttack $ enemyAttack (toId attrs) iid
        pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Flip iid (toSource attrs) (toTarget attrs)
        pure e
    Flip iid _ target | isTarget attrs target -> do
      push $ ReadStory iid Story.josefsPlan
      pure e
    ResolveStory _ story' | story' == Story.josefsPlan -> do
      pushAll [createCardEffect Cards.josefMeiger Nothing attrs attrs, RemoveAllDoom (toTarget attrs), DisengageEnemyFromAll (toId attrs)]
      pure e
    _ -> JosefMeiger <$> runMessage msg attrs

newtype JosefMeigerEffect = JosefMeigerEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

josefMeigerEffect :: EffectArgs -> JosefMeigerEffect
josefMeigerEffect =
  cardEffect JosefMeigerEffect Cards.josefMeiger

instance HasModifiersFor JosefMeigerEffect where
  getModifiersFor (EnemyTarget eid) (JosefMeigerEffect a) = do
    isSilverTwilight <- eid <=~> EnemyWithTrait SilverTwilight
    isJosefMeiger <- eid <=~> enemyIs Cards.josefMeiger
    pure $ toModifiers a $ [CannotPlaceDoomOnThis | isSilverTwilight] <> [AddKeyword Keyword.Aloof | isJosefMeiger]
  getModifiersFor _ _ = pure []

instance RunMessage JosefMeigerEffect where
  runMessage msg (JosefMeigerEffect attrs) = JosefMeigerEffect <$> runMessage msg attrs
