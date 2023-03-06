module Arkham.Enemy.Cards.TheSpectralWatcher
  ( theSpectralWatcher
  , TheSpectralWatcher(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Timing qualified as Timing

newtype TheSpectralWatcher = TheSpectralWatcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSpectralWatcher :: EnemyCard TheSpectralWatcher
theSpectralWatcher =
  enemy TheSpectralWatcher Cards.theSpectralWatcher (3, Static 5, 3) (1, 1)

instance HasAbilities TheSpectralWatcher where
  getAbilities (TheSpectralWatcher a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyDefeated Timing.When Anyone
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage TheSpectralWatcher where
  runMessage msg e@(TheSpectralWatcher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ CancelNext (toSource attrs) EnemyDefeatedMessage
        , HealAllDamage (toTarget attrs) (toSource attrs)
        , DisengageEnemyFromAll (toId attrs)
        , Exhaust (toTarget attrs)
        , CreateWindowModifierEffect
          EffectRoundWindow
          (EffectModifiers $ toModifiers attrs [DoesNotReadyDuringUpkeep])
          (toSource attrs)
          (toTarget attrs)
        ]
      pure e
    _ -> TheSpectralWatcher <$> runMessage msg attrs
