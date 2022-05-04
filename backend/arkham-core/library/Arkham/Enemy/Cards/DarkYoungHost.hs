module Arkham.Enemy.Cards.DarkYoungHost
  ( darkYoungHost
  , DarkYoungHost(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Target
import qualified Arkham.Timing as Timing
import Arkham.Trait
import Arkham.Window (Window(..))
import qualified Arkham.Window as Window

newtype DarkYoungHost = DarkYoungHost EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkYoungHost :: EnemyCard DarkYoungHost
darkYoungHost = enemyWith
  DarkYoungHost
  Cards.darkYoungHost
  (4, Static 5, 2)
  (2, 2)
  (spawnAtL ?~ LocationWithTrait Bayou)

instance HasAbilities DarkYoungHost where
  getAbilities (DarkYoungHost attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1 $ ForcedAbility $ PlacedCounterOnLocation
            Timing.When
            (LocationWithId loc)
            ClueCounter
            (AtLeast $ Static 1)
        | loc <- maybeToList (enemyLocation attrs)
        ]
      <> [ mkAbility attrs 2
           $ ForcedAbility
           $ EnemyDefeated Timing.When Anyone
           $ EnemyWithId
           $ toId attrs
         ]

instance EnemyRunner env => RunMessage env DarkYoungHost where
  runMessage msg e@(DarkYoungHost attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.PlacedClues target n)] 1 _
      | isSource attrs source -> do
        e <$ pushAll [RemoveClues target n, PlaceClues (toTarget attrs) n]
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      case enemyLocation attrs of
        Nothing -> pure e
        Just loc ->
          e <$ push (PlaceClues (LocationTarget loc) (enemyClues attrs))
    _ -> DarkYoungHost <$> runMessage msg attrs
