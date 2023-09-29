module Arkham.Effect.Effects.QuickThinking (
  QuickThinking (..),
  quickThinking,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner

newtype QuickThinking = QuickThinking EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: EffectArgs -> QuickThinking
quickThinking = QuickThinking . uncurry4 (baseAttrs "02229")

instance RunMessage QuickThinking where
  runMessage msg e@(QuickThinking attrs) = case msg of
    AfterSkillTestEnds {} -> case effectTarget attrs of
      InvestigatorTarget iid ->
        e <$ pushAll [DisableEffect (toId attrs), PlayerWindow iid [] True]
      _ -> error "wrong target"
    _ -> QuickThinking <$> runMessage msg attrs
