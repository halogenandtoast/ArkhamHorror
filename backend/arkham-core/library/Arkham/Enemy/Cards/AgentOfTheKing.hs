module Arkham.Enemy.Cards.AgentOfTheKing (
  agentOfTheKing,
  AgentOfTheKing (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype AgentOfTheKing = AgentOfTheKing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

agentOfTheKing :: EnemyCard AgentOfTheKing
agentOfTheKing =
  enemyWith AgentOfTheKing Cards.agentOfTheKing (4, Static 4, 2) (1, 2)
    $ preyL
    .~ Prey MostClues

instance HasAbilities AgentOfTheKing where
  getAbilities (AgentOfTheKing a) =
    withBaseAbilities a
      $ [ mkAbility a 1
            $ ForcedAbility
            $ EnemyAttacks #after (You <> InvestigatorWithAnyClues) AnyEnemyAttack
            $ be a
        , mkAbility a 2 $ ForcedAbility $ EnemyDefeated #when You ByAny $ be a <> EnemyWithAnyClues
        ]

instance RunMessage AgentOfTheKing where
  runMessage msg e@(AgentOfTheKing attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [InvestigatorSpendClues iid 1, PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1]
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      pushAll
        [ RemoveClues (toAbilitySource attrs 2) (toTarget attrs) (enemyClues attrs)
        , GainClues iid (toAbilitySource attrs 2) (enemyClues attrs)
        ]
      pure e
    _ -> AgentOfTheKing <$> runMessage msg attrs
