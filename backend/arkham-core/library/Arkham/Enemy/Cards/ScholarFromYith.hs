module Arkham.Enemy.Cards.ScholarFromYith
  ( scholarFromYith
  , ScholarFromYith(..)
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
import Arkham.Matcher hiding ( EnemyEvaded )
import Arkham.Message hiding ( EnemyAttacks )
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ScholarFromYith = ScholarFromYith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scholarFromYith :: EnemyCard ScholarFromYith
scholarFromYith = enemyWith
  ScholarFromYith
  Cards.scholarFromYith
  (2, Static 2, 2)
  (0, 1)
  (preyL .~ Prey MostCardsInHand)

instance HasAbilities ScholarFromYith where
  getAbilities (ScholarFromYith a) = withBaseAbilities
    a
    [ mkAbility a 1
    $ ForcedAbility
    $ EnemyAttacks Timing.When You AnyEnemyAttack
    $ EnemyWithId
    $ toId a
    , restrictedAbility
      a
      2
      (EnemyCriteria $ EnemyExists $ EnemyIsEngagedWith You <> ReadyEnemy)
    $ ActionAbility (Just Action.Parley)
    $ ActionCost 1
    ]

instance RunMessage ScholarFromYith where
  runMessage msg e@(ScholarFromYith attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll [RandomDiscard iid, RandomDiscard iid]
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ BeginSkillTest
        iid
        (toAbilitySource attrs 2)
        (InvestigatorTarget iid)
        (Just Action.Parley)
        SkillIntellect
        3
      pure e
    PassedSkillTest iid _ (isAbilitySource attrs 2 -> True) SkillTestInitiatorTarget{} _ _
      -> do
        pushAll [drawCards iid attrs 1, EnemyEvaded iid (toId attrs)]
        pure e
    FailedSkillTest iid _ (isAbilitySource attrs 2 -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ InitiateEnemyAttack iid (toId attrs) RegularAttack
        pure e
    _ -> ScholarFromYith <$> runMessage msg attrs
