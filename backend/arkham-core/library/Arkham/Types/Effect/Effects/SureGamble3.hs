module Arkham.Types.Effect.Effects.SureGamble3
  ( sureGamble3
  , SureGamble3(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier

newtype SureGamble3 = SureGamble3 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EffectArgs -> SureGamble3
sureGamble3 = SureGamble3 . uncurry4 (baseAttrs "01088")

instance HasModifiersFor env SureGamble3 where
  getModifiersFor _ target (SureGamble3 a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a NegativeToPositive]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env SureGamble3 where
  runMessage msg e@(SureGamble3 attrs) = case msg of
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> SureGamble3 <$> runMessage msg attrs
