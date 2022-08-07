module Arkham.Effect.Effects.SureGamble3
  ( sureGamble3
  , SureGamble3(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message

newtype SureGamble3 = SureGamble3 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EffectArgs -> SureGamble3
sureGamble3 = SureGamble3 . uncurry4 (baseAttrs "01088")

instance HasModifiersFor SureGamble3 where
  getModifiersFor target (SureGamble3 a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a NegativeToPositive]
  getModifiersFor _ _ = pure []

instance RunMessage SureGamble3 where
  runMessage msg e@(SureGamble3 attrs) = case msg of
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> SureGamble3 <$> runMessage msg attrs
