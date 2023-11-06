module Arkham.Event.Cards.SpectralRazor (
  spectralRazor,
  spectralRazorEffect,
  SpectralRazor (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype SpectralRazor = SpectralRazor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazor :: EventCard SpectralRazor
spectralRazor = event SpectralRazor Cards.spectralRazor

instance RunMessage SpectralRazor where
  runMessage msg e@(SpectralRazor attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs iid (AddSkillValue #willpower)
        , createCardEffect Cards.spectralRazor Nothing attrs iid
        , chooseFightEnemy iid attrs #combat
        ]
      pure e
    _ -> SpectralRazor <$> runMessage msg attrs

newtype SpectralRazorEffect = SpectralRazorEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazorEffect :: EffectArgs -> SpectralRazorEffect
spectralRazorEffect = cardEffect SpectralRazorEffect Cards.spectralRazor

instance HasModifiersFor SpectralRazorEffect where
  getModifiersFor target (SpectralRazorEffect a) | effectTarget a == target = do
    mtarget <- getSkillTestTarget
    case mtarget of
      Just (EnemyTarget eid) -> do
        elite <- eid <=~> EliteEnemy
        pure $ toModifiers a [DamageDealt $ if elite then 1 else 2]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage SpectralRazorEffect where
  runMessage msg e@(SpectralRazorEffect attrs) = case msg of
    SkillTestEnds {} -> do
      push $ disable attrs
      pure e
    _ -> SpectralRazorEffect <$> runMessage msg attrs
