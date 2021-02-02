module Arkham.Types.Effect.Effects.WindowModifierEffect
  ( windowModifierEffect
  , WindowModifierEffect(..)
  ) where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype WindowModifierEffect = WindowModifierEffect Attrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

windowModifierEffect
  :: EffectId
  -> EffectMetadata Message
  -> EffectWindow
  -> Source
  -> Target
  -> WindowModifierEffect
windowModifierEffect eid metadata effectWindow source target =
  WindowModifierEffect $ Attrs
    { effectId = eid
    , effectSource = source
    , effectTarget = target
    , effectCardCode = Nothing
    , effectMetadata = Just metadata
    , effectTraits = mempty
    , effectWindow = Just effectWindow
    }

instance HasModifiersFor env WindowModifierEffect where
  getModifiersFor _ target (WindowModifierEffect Attrs {..})
    | target == effectTarget = case effectMetadata of
      Just (EffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env WindowModifierEffect where
  runMessage msg (WindowModifierEffect attrs) =
    WindowModifierEffect <$> runMessage msg attrs
