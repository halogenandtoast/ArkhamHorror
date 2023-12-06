module Arkham.Enemy.Cards.TheWingedSerpent (
  theWingedSerpent,
  TheWingedSerpent (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype TheWingedSerpent = TheWingedSerpent EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWingedSerpent :: EnemyCard TheWingedSerpent
theWingedSerpent =
  enemyWith
    TheWingedSerpent
    Cards.theWingedSerpent
    (8, Static 0, 5)
    (1, 1)
    ((spawnAtL ?~ SpawnAt (LocationWithTitle "Mouth of K'n-yan")) . (healthL .~ Nothing))

instance HasModifiersFor TheWingedSerpent where
  getModifiersFor target (TheWingedSerpent a)
    | isTarget a target =
        pure $ toModifiers a [CannotBeDefeated, CannotMakeAttacksOfOpportunity]
  getModifiersFor _ _ = pure []

instance HasAbilities TheWingedSerpent where
  getAbilities (TheWingedSerpent a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ PlacedCounterOnLocation
            Timing.After
            (LocationWithTitle "Mouth of K'n-yan")
            AnySource
            ResourceCounter
            AnyValue
      ]

instance RunMessage TheWingedSerpent where
  runMessage msg e@(TheWingedSerpent attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ Exhaust (toTarget attrs)
        , roundModifier attrs attrs DoesNotReadyDuringUpkeep
        ]
      pure e
    _ -> TheWingedSerpent <$> runMessage msg attrs
