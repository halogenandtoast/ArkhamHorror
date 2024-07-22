module Arkham.Enemy.Cards.NathanWickMasterOfInitiation (
  nathanWickMasterOfInitiation,
  NathanWickMasterOfInitiation (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.SkillType

newtype NathanWickMasterOfInitiation = NathanWickMasterOfInitiation EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanWickMasterOfInitiation :: EnemyCard NathanWickMasterOfInitiation
nathanWickMasterOfInitiation = enemy NathanWickMasterOfInitiation Cards.nathanWickMasterOfInitiation (3, Static 5, 4) (1, 1)

instance HasAbilities NathanWickMasterOfInitiation where
  getAbilities (NathanWickMasterOfInitiation attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 OnSameLocation
          $ ActionAbility [Action.Parley]
          $ ActionCost 1
      ]

instance RunMessage NathanWickMasterOfInitiation where
  runMessage msg e@(NathanWickMasterOfInitiation attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ parley sid iid (toAbilitySource attrs 1) iid SkillWillpower (Fixed 3)
      pure e
    PassedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      n <- perPlayer 1
      pushAll
        $ PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1
        : [AddToVictory (toTarget attrs) | enemyResources attrs + 1 >= n]
      pure e
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    _ -> NathanWickMasterOfInitiation <$> runMessage msg attrs
