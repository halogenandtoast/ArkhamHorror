module Arkham.Enemy.Cards.RoyalEmissary (
  royalEmissary,
  RoyalEmissary (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype RoyalEmissary = RoyalEmissary EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

royalEmissary :: EnemyCard RoyalEmissary
royalEmissary =
  enemyWith RoyalEmissary Cards.royalEmissary (4, Static 4, 2) (2, 0)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #willpower)

investigatorMatcher :: EnemyAttrs -> InvestigatorMatcher
investigatorMatcher a =
  AnyInvestigator
    [ InvestigatorAt $ locationWithEnemy (toId a)
    , InvestigatorAt $ AccessibleFrom $ locationWithEnemy (toId a)
    ]

instance HasAbilities RoyalEmissary where
  getAbilities (RoyalEmissary a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 (InvestigatorExists $ investigatorMatcher a)
            $ ForcedAbility
            $ PhaseEnds #when #enemy
        ]

instance RunMessage RoyalEmissary where
  runMessage msg e@(RoyalEmissary attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      investigators <- select $ investigatorMatcher attrs
      pushAll $ map (\investigator -> assignHorror investigator source 1) investigators
      pure e
    _ -> RoyalEmissary <$> runMessage msg attrs
