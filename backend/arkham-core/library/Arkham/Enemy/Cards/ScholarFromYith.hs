module Arkham.Enemy.Cards.ScholarFromYith (
  scholarFromYith,
  ScholarFromYith (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message hiding (EnemyAttacks)
import Arkham.Timing qualified as Timing

newtype ScholarFromYith = ScholarFromYith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scholarFromYith :: EnemyCard ScholarFromYith
scholarFromYith =
  enemyWith
    ScholarFromYith
    Cards.scholarFromYith
    (2, Static 2, 2)
    (0, 1)
    (preyL .~ Prey MostCardsInHand)

instance HasAbilities ScholarFromYith where
  getAbilities (ScholarFromYith a) =
    withBaseAbilities
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
      pushAll
        [ toMessage $ randomDiscard iid (toAbilitySource attrs 1)
        , toMessage $ randomDiscard iid (toAbilitySource attrs 1)
        ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ parley iid (toAbilitySource attrs 2) iid #intellect 3
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      drawing <- drawCards iid (toAbilitySource attrs 2) 1
      pushAll [drawing, EnemyEvaded iid (toId attrs)]
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toMessage $ enemyAttack (toId attrs) (toAbilitySource attrs 2) iid
      pure e
    _ -> ScholarFromYith <$> runMessage msg attrs
