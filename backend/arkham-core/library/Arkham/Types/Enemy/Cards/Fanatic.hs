module Arkham.Types.Enemy.Cards.Fanatic
  ( fanatic
  , Fanatic(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype Fanatic = Fanatic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fanatic :: EnemyCard Fanatic
fanatic = enemyWith
  Fanatic
  Cards.fanatic
  (3, Static 2, 3)
  (1, 0)
  (spawnAtL ?~ LocationWithMostClues RevealedLocation)

instance HasAbilities Fanatic where
  getAbilities (Fanatic a) = withBaseAbilities
    a
    [ mkAbility a 1
    $ ForcedAbility
    $ EnemySpawns Timing.After LocationWithAnyClues
    $ EnemyWithId
    $ toId a
    , mkAbility a 2
    $ ForcedAbility
    $ EnemyDefeated Timing.When You
    $ EnemyWithId (toId a)
    <> EnemyWithAnyClues
    ]

instance EnemyRunner env => RunMessage env Fanatic where
  runMessage msg e@(Fanatic attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      e <$ pushAll
        [ RemoveClues (LocationTarget $ enemyLocation attrs) 1
        , PlaceClues (toTarget attrs) 1
        ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      e <$ pushAll
        [ RemoveClues (toTarget attrs) (enemyClues attrs)
        , GainClues iid (enemyClues attrs)
        ]
    _ -> Fanatic <$> runMessage msg attrs
