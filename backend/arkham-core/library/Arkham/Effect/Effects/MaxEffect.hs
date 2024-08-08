module Arkham.Effect.Effects.MaxEffect (MaxEffect (..), maxEffect) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Prelude

newtype MaxEffect = MaxEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maxEffect :: EffectArgs -> MaxEffect
maxEffect = MaxEffect . uncurry (baseAttrs "maxef")

instance HasModifiersFor MaxEffect

instance RunMessage MaxEffect where
  runMessage msg (MaxEffect attrs) =
    MaxEffect <$> runMessage msg attrs
