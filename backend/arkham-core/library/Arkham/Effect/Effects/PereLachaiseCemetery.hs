module Arkham.Effect.Effects.PereLachaiseCemetery (
  PereLachaiseCemetery (..),
  pereLachaiseCemetery,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers

newtype PereLachaiseCemetery = PereLachaiseCemetery EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

pereLachaiseCemetery :: EffectArgs -> PereLachaiseCemetery
pereLachaiseCemetery = PereLachaiseCemetery . uncurry4 (baseAttrs "03215")

instance HasModifiersFor PereLachaiseCemetery where
  getModifiersFor target (PereLachaiseCemetery attrs)
    | target == effectTarget attrs = pure $ toModifiers attrs [CannotMove]
  getModifiersFor _ _ = pure []

instance RunMessage PereLachaiseCemetery where
  runMessage msg e@(PereLachaiseCemetery attrs) = case msg of
    EndRoundWindow -> e <$ push (DisableEffect $ toId attrs)
    _ -> PereLachaiseCemetery <$> runMessage msg attrs
