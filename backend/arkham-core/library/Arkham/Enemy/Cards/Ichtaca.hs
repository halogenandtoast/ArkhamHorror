module Arkham.Enemy.Cards.Ichtaca
  ( ichtaca
  , Ichtaca(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Message
import Arkham.SkillType

newtype Ichtaca = Ichtaca EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtaca :: EnemyCard Ichtaca
ichtaca = enemy Ichtaca Cards.ichtaca (5, Static 4, 4) (2, 0)

instance HasAbilities Ichtaca where
  getAbilities (Ichtaca a) = withBaseAbilities
    a
    [ restrictedAbility a 1 OnSameLocation
      $ ActionAbility (Just Action.Parley)
      $ ActionCost 1
    ]

instance RunMessage Ichtaca where
  runMessage msg e@(Ichtaca attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ parley
        iid
        source
        (toTarget attrs)
        SkillIntellect
        4
      pure e
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        push $ PlaceClues (toTarget attrs) 1
        pure e
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        push $ InitiateEnemyAttack iid (toId attrs) RegularAttack
        pure e
    _ -> Ichtaca <$> runMessage msg attrs
