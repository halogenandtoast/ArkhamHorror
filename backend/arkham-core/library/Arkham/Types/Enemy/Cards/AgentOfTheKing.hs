module Arkham.Types.Enemy.Cards.AgentOfTheKing
  ( agentOfTheKing
  , AgentOfTheKing(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Types.Prey
import qualified Arkham.Types.Timing as Timing

newtype AgentOfTheKing = AgentOfTheKing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentOfTheKing :: EnemyCard AgentOfTheKing
agentOfTheKing = enemyWith
  AgentOfTheKing
  Cards.agentOfTheKing
  (4, Static 4, 2)
  (1, 2)
  (preyL .~ MostClues)

instance HasAbilities AgentOfTheKing where
  getAbilities (AgentOfTheKing a) = withBaseAbilities
    a
    [ mkAbility a 1
    $ ForcedAbility
    $ EnemyAttacks Timing.After (You <> InvestigatorWithAnyClues)
    $ EnemyWithId
    $ toId a
    , mkAbility a 2
    $ ForcedAbility
    $ EnemyDefeated Timing.When You
    $ EnemyWithId (toId a)
    <> EnemyWithAnyClues
    ]

instance EnemyRunner env => RunMessage env AgentOfTheKing where
  runMessage msg e@(AgentOfTheKing attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ pushAll [InvestigatorSpendClues iid 1, PlaceClues (toTarget attrs) 1]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> e <$ pushAll
      [ RemoveClues (toTarget attrs) (enemyClues attrs)
      , GainClues iid (enemyClues attrs)
      ]
    _ -> AgentOfTheKing <$> runMessage msg attrs
