module Arkham.Enemy.Cards.HasturTheKingInYellow
  ( hasturTheKingInYellow
  , HasturTheKingInYellow(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack hiding ( damageStrategyL )
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Phase
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype HasturTheKingInYellow = HasturTheKingInYellow EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hasturTheKingInYellow :: EnemyCard HasturTheKingInYellow
hasturTheKingInYellow = enemyWith
  HasturTheKingInYellow
  Cards.hasturTheKingInYellow
  (4, PerPlayer 7, 2)
  (0, 2)
  (damageStrategyL .~ DamageFromHastur)

instance HasAbilities HasturTheKingInYellow where
  getAbilities (HasturTheKingInYellow a) = withBaseAbilities
    a
    [ restrictedAbility
      a
      1
      (EnemyCriteria $ EnemyExists $ EnemyWithId (toId a) <> ReadyEnemy)
    $ ForcedAbility
    $ PhaseBegins Timing.When
    $ PhaseIs EnemyPhase
    , mkAbility a 2 $ ActionAbility Nothing $ ActionCost 1
    ]

instance RunMessage HasturTheKingInYellow where
  runMessage msg e@(HasturTheKingInYellow attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      iids <- getInvestigatorIds
      pushAll $ map (InitiateEnemyAttack . enemyAttack (toId attrs)) iids
      pure e
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      x <- field EnemyFight (toId attrs)
      push $ beginSkillTest iid source (toTarget attrs) SkillWillpower x
      pure e
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        push $ Exhaust (toTarget attrs)
        pure e
    Msg.EnemyDamage eid assignment
      | eid
        == toId attrs
        && damageAssignmentDamageEffect assignment
        == StoryCardDamageEffect
      -> HasturTheKingInYellow <$> runMessage msg attrs
    Msg.EnemyDamage eid _ | eid == toId attrs -> pure e
    _ -> HasturTheKingInYellow <$> runMessage msg attrs
