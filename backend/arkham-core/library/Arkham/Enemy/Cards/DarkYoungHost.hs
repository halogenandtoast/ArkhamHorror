module Arkham.Enemy.Cards.DarkYoungHost (
  darkYoungHost,
  DarkYoungHost (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DarkYoungHost = DarkYoungHost EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkYoungHost :: EnemyCard DarkYoungHost
darkYoungHost =
  enemyWith
    DarkYoungHost
    Cards.darkYoungHost
    (4, Static 5, 2)
    (2, 2)
    $ spawnAtL
    ?~ SpawnAt (LocationWithTrait Bayou)

instance HasAbilities DarkYoungHost where
  getAbilities (DarkYoungHost attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
            $ ForcedAbility
            $ PlacedCounterOnLocation
              Timing.When
              LocationOfThis
              AnySource
              ClueCounter
              (AtLeast $ Static 1)
        , mkAbility attrs 2
            $ ForcedAbility
            $ EnemyDefeated Timing.When Anyone ByAny
            $ EnemyWithId
            $ toId attrs
        ]

instance RunMessage DarkYoungHost where
  runMessage msg e@(DarkYoungHost attrs) = case msg of
    UseCardAbility _ source 1 [(windowType -> Window.PlacedClues _ target n)] _ | isSource attrs source -> do
      pushAll
        [ RemoveClues (toAbilitySource attrs 1) target n
        , PlaceClues (toAbilitySource attrs 1) (toTarget attrs) n
        ]
      pure e
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      mEnemyLocation <- field EnemyLocation (toId attrs)
      for_ mEnemyLocation $ \loc ->
        push $ PlaceClues (toAbilitySource attrs 2) (toTarget loc) (enemyClues attrs)
      pure e
    _ -> DarkYoungHost <$> runMessage msg attrs
