module Arkham.Types.Effect.Effects.PhaseEffect
  ( phaseEffect
  , PhaseEffect(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype PhaseEffect = PhaseEffect Attrs
  deriving newtype (Show, ToJSON, FromJSON)

phaseEffect
  :: EffectId -> EffectMetadata Message -> Source -> Target -> PhaseEffect
phaseEffect eid metadata source target = PhaseEffect $ Attrs
  { effectId = eid
  , effectSource = source
  , effectTarget = target
  , effectCardCode = Nothing
  , effectMetadata = Just metadata
  }

instance HasModifiersFor env PhaseEffect where
  getModifiersFor _ target (PhaseEffect Attrs {..}) | target == effectTarget =
    case effectMetadata of
      Just (EffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env PhaseEffect where
  runMessage msg e@(PhaseEffect attrs) = case msg of
    EndPhase -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> PhaseEffect <$> runMessage msg attrs
