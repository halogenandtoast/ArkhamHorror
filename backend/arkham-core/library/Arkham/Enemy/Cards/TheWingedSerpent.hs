module Arkham.Enemy.Cards.TheWingedSerpent
  ( theWingedSerpent
  , TheWingedSerpent(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype TheWingedSerpent = TheWingedSerpent EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWingedSerpent :: EnemyCard TheWingedSerpent
theWingedSerpent = enemyWith
  TheWingedSerpent
  Cards.theWingedSerpent
  (8, Static 0, 5)
  (1, 1)
  (spawnAtL ?~ LocationWithTitle "Mouth of K'n-yan")

instance HasModifiersFor TheWingedSerpent where
  getModifiersFor target (TheWingedSerpent a) | isTarget a target =
    pure $ toModifiers a [CannotBeDefeated, CannotMakeAttacksOfOpportunity]
  getModifiersFor _ _ = pure []

instance HasAbilities TheWingedSerpent where
  getAbilities (TheWingedSerpent a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ PlacedCounterOnLocation
        Timing.After
        (LocationWithTitle "Mouth of K'n-yan")
        ResourceCounter
        AnyValue
    ]

instance RunMessage TheWingedSerpent where
  runMessage msg e@(TheWingedSerpent attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ Exhaust (toTarget attrs)
        , CreateWindowModifierEffect
          EffectRoundWindow
          (EffectModifiers $ toModifiers attrs [DoesNotReadyDuringUpkeep])
          (toSource attrs)
          (toTarget attrs)
        ]
      pure e
    _ -> TheWingedSerpent <$> runMessage msg attrs
