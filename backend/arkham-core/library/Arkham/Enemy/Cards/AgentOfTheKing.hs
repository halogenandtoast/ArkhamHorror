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
import Arkham.Message hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Timing qualified as Timing

newtype AgentOfTheKing = AgentOfTheKing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentOfTheKing :: EnemyCard AgentOfTheKing
agentOfTheKing =
  enemyWith
    AgentOfTheKing
    Cards.agentOfTheKing
    (4, Static 4, 2)
    (1, 2)
    (preyL .~ Prey MostClues)

instance HasAbilities AgentOfTheKing where
  getAbilities (AgentOfTheKing a) =
    withBaseAbilities
      a
      [ mkAbility a 1 $
          ForcedAbility $
            EnemyAttacks Timing.After (You <> InvestigatorWithAnyClues) AnyEnemyAttack $
              EnemyWithId $
                toId a
      , mkAbility a 2 $
          ForcedAbility $
            EnemyDefeated Timing.When You ByAny $
              EnemyWithId (toId a)
                <> EnemyWithAnyClues
      ]

instance RunMessage AgentOfTheKing where
  runMessage msg e@(AgentOfTheKing attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll [InvestigatorSpendClues iid 1, PlaceClues (toTarget attrs) 1]
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      pushAll
        [ RemoveClues (toTarget attrs) (enemyClues attrs)
        , GainClues iid (enemyClues attrs)
        ]
      pure e
    _ -> AgentOfTheKing <$> runMessage msg attrs
