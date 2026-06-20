module Arkham.Enemy.Cards.HasturTheKingInYellow (hasturTheKingInYellow) where

import Arkham.Ability
import Arkham.Attack.Types (EnemyAttackDetails (..))
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Placement (Placement (Global))
import Arkham.Strategy

newtype HasturTheKingInYellow = HasturTheKingInYellow EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor HasturTheKingInYellow where
  getModifiersFor (HasturTheKingInYellow a) =
    modifySelfWhen a (a.placement == Global) [CannotBeAttacked]

hasturTheKingInYellow :: EnemyCard HasturTheKingInYellow
hasturTheKingInYellow =
  enemyWith HasturTheKingInYellow Cards.hasturTheKingInYellow
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
      eachInvestigator \i ->
        initiateEnemyAttackEdit attrs (attrs.ability 1) i \d -> d {attackDamageStrategy = DamageFromHastur}
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (toAbilitySource attrs 2) attrs #willpower
        $ EnemyMaybeFieldCalculation attrs.id Field.EnemyFight
      pure e
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      exhaustThis attrs
      pure e
    Msg.DealDamage (EnemyTarget eid) assignment
      | eid == toId attrs
      , damageAssignmentDamageEffect assignment == StoryCardDamageEffect -> do
          HasturTheKingInYellow <$> liftRunMessage msg attrs
    Msg.DealDamage (EnemyTarget eid) _ | eid == toId attrs -> pure e
    _ -> HasturTheKingInYellow <$> liftRunMessage msg attrs
