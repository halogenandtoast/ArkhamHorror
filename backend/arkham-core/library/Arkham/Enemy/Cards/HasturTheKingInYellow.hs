module Arkham.Enemy.Cards.HasturTheKingInYellow (
  hasturTheKingInYellow,
  HasturTheKingInYellow (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack hiding (damageStrategyL)
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection

newtype HasturTheKingInYellow = HasturTheKingInYellow EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hasturTheKingInYellow :: EnemyCard HasturTheKingInYellow
hasturTheKingInYellow =
  enemyWith HasturTheKingInYellow Cards.hasturTheKingInYellow (4, PerPlayer 7, 2) (0, 2)
    $ damageStrategyL
    .~ DamageFromHastur

instance HasAbilities HasturTheKingInYellow where
  getAbilities (HasturTheKingInYellow a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 (enemyExists $ EnemyWithId (toId a) <> ReadyEnemy)
            $ ForcedAbility
            $ PhaseBegins #when #enemy
        , mkAbility a 2 $ ActionAbility [] $ ActionCost 1
        ]

instance RunMessage HasturTheKingInYellow where
  runMessage msg e@(HasturTheKingInYellow attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- getInvestigatorIds
      pushAll $ map (InitiateEnemyAttack . enemyAttack (toId attrs) attrs) iids
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      x <- field EnemyFight (toId attrs)
      push $ beginSkillTest iid (toAbilitySource attrs 2) attrs #willpower x
      pure e
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      push $ Exhaust (toTarget attrs)
      pure e
    Msg.EnemyDamage eid assignment
      | eid == toId attrs
      , damageAssignmentDamageEffect assignment == StoryCardDamageEffect -> do
          HasturTheKingInYellow <$> runMessage msg attrs
    Msg.EnemyDamage eid _ | eid == toId attrs -> pure e
    _ -> HasturTheKingInYellow <$> runMessage msg attrs
