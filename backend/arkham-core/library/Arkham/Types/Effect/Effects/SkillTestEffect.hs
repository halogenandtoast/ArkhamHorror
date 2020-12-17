module Arkham.Types.Effect.Effects.SkillTestEffect
  ( skillTestEffect
  , SkillTestEffect(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype SkillTestEffect = SkillTestEffect Attrs
  deriving newtype (Show, ToJSON, FromJSON)

skillTestEffect
  :: EffectId -> EffectMetadata Message -> Source -> Target -> SkillTestEffect
skillTestEffect eid metadata source target = SkillTestEffect $ Attrs
  { effectId = eid
  , effectSource = source
  , effectTarget = target
  , effectCardCode = Nothing
  , effectMetadata = Just metadata
  , effectTraits = mempty
  }

instance HasModifiersFor env SkillTestEffect where
  getModifiersFor _ target (SkillTestEffect Attrs {..})
    | target == effectTarget = case effectMetadata of
      Just (EffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env SkillTestEffect where
  runMessage msg e@(SkillTestEffect attrs) = case msg of
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> SkillTestEffect <$> runMessage msg attrs
