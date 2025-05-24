module Arkham.Enemy.Cards.HasturTheKingInYellow (hasturTheKingInYellow) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyFight))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Strategy

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
    extend
      a
      [ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseBegins #when #enemy
      , skillTestAbility $ mkAbility a 2 actionAbility
      ]

instance RunMessage HasturTheKingInYellow where
  runMessage msg e@(HasturTheKingInYellow attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (toAbilitySource attrs 2) attrs #willpower
        $ EnemyMaybeFieldCalculation attrs.id EnemyFight
      pure e
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      exhaustThis attrs
      pure e
    Msg.EnemyDamage eid assignment
      | eid == toId attrs
      , damageAssignmentDamageEffect assignment == StoryCardDamageEffect -> do
          HasturTheKingInYellow <$> liftRunMessage msg attrs
    Msg.EnemyDamage eid _ | eid == toId attrs -> pure e
    _ -> HasturTheKingInYellow <$> liftRunMessage msg attrs
