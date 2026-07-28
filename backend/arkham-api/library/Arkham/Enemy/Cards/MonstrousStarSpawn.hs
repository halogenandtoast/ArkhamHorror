module Arkham.Enemy.Cards.MonstrousStarSpawn (monstrousStarSpawn) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype MonstrousStarSpawn = MonstrousStarSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monstrousStarSpawn :: EnemyCard MonstrousStarSpawn
monstrousStarSpawn =
  enemyWith
    MonstrousStarSpawn
    Cards.monstrousStarSpawn
    (preyL .~ Prey (InvestigatorWithHighestSkill #combat UneliminatedInvestigator))

instance HasAbilities MonstrousStarSpawn where
  getAbilities (MonstrousStarSpawn a) =
    extend1 a
      $ restricted
        a
        1
        ( DuringSkillTest (SkillTestOneOf [WhileAttackingAnEnemy $ be a, WhileEvadingAnEnemy $ be a])
            <> thisExists a EnemyWithAnyDamage
        )
      $ forced
      $ RevealChaosToken #after You #elderthing

instance RunMessage MonstrousStarSpawn where
  runMessage msg e@(MonstrousStarSpawn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> MonstrousStarSpawn <$> liftRunMessage msg attrs
