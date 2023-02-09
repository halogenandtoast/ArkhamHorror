module Arkham.Effect.Effects.AChanceEncounter
  ( aChanceEncounter
  , AChanceEncounter(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.Target

newtype AChanceEncounter = AChanceEncounter EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter :: EffectArgs -> AChanceEncounter
aChanceEncounter = AChanceEncounter . uncurry4 (baseAttrs "02270")

instance RunMessage AChanceEncounter where
  runMessage msg e@(AChanceEncounter attrs@EffectAttrs {..}) = case msg of
    EndRoundWindow -> case effectTarget of
      CardIdTarget cardId ->
        e <$ pushAll [Discard effectSource $ CardIdTarget cardId, DisableEffect effectId]
      _ -> error "Wrong target type"
    _ -> AChanceEncounter <$> runMessage msg attrs
