module Arkham.Enemy.Cards.DarkYoungHost
  ( darkYoungHost
  , DarkYoungHost(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EnemyDefeated )
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype DarkYoungHost = DarkYoungHost EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkYoungHost :: EnemyCard DarkYoungHost
darkYoungHost = enemyWith
  DarkYoungHost
  Cards.darkYoungHost
  (4, Static 5, 2)
  (2, 2)
  (spawnAtL ?~ SpawnLocation (LocationWithTrait Bayou))

instance HasAbilities DarkYoungHost where
  getAbilities (DarkYoungHost attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1 $ ForcedAbility $ PlacedCounterOnLocation
          Timing.When
          LocationOfThis
          ClueCounter
          (AtLeast $ Static 1)
        , mkAbility attrs 2
        $ ForcedAbility
        $ EnemyDefeated Timing.When Anyone
        $ EnemyWithId
        $ toId attrs
        ]

instance RunMessage DarkYoungHost where
  runMessage msg e@(DarkYoungHost attrs) = case msg of
    UseCardAbility _ source 1 [Window _ (Window.PlacedClues target n)] _
      | isSource attrs source -> do
        e <$ pushAll [RemoveClues target n, PlaceClues (toTarget attrs) n]
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      case enemyLocation of
        Nothing -> pure e
        Just loc ->
          e <$ push (PlaceClues (LocationTarget loc) (enemyClues attrs))
    _ -> DarkYoungHost <$> runMessage msg attrs
