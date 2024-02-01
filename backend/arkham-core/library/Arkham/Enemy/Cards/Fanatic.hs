module Arkham.Enemy.Cards.Fanatic (
  fanatic,
  Fanatic (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype Fanatic = Fanatic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

fanatic :: EnemyCard Fanatic
fanatic =
  enemyWith
    Fanatic
    Cards.fanatic
    (3, Static 2, 3)
    (1, 0)
    (spawnAtL ?~ SpawnAt (LocationWithMostClues RevealedLocation))

instance HasAbilities Fanatic where
  getAbilities (Fanatic a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemySpawns Timing.After LocationWithAnyClues
          $ EnemyWithId
          $ toId a
      , mkAbility a 2
          $ ForcedAbility
          $ EnemyDefeated Timing.When You ByAny
          $ EnemyWithId (toId a)
          <> EnemyWithAnyClues
      ]

instance RunMessage Fanatic where
  runMessage msg e@(Fanatic attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      for_ enemyLocation $ \loc ->
        pushAll
          [ RemoveClues (toAbilitySource attrs 1) (LocationTarget loc) 1
          , PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
          ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      pushAll
        [ RemoveClues (toAbilitySource attrs 2) (toTarget attrs) (enemyClues attrs)
        , GainClues iid (toAbilitySource attrs 2) (enemyClues attrs)
        ]
      pure e
    _ -> Fanatic <$> runMessage msg attrs
