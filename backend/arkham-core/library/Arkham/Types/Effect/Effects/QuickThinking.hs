module Arkham.Types.Effect.Effects.QuickThinking
  ( QuickThinking(..)
  , quickThinking
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

newtype QuickThinking = QuickThinking EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: EffectArgs -> QuickThinking
quickThinking = QuickThinking . uncurry4 (baseAttrs "02229")

instance HasModifiersFor env QuickThinking

instance HasQueue env => RunMessage env QuickThinking where
  runMessage msg e@(QuickThinking attrs) = case msg of
    AfterSkillTestEnds -> case effectTarget attrs of
      InvestigatorTarget iid ->
        e <$ pushAll [DisableEffect (toId attrs), PlayerWindow iid [] True]
      _ -> error "wrong target"
    _ -> QuickThinking <$> runMessage msg attrs

