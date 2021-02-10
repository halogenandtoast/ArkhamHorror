module Arkham.Types.Effect.Effects.WindowModifierEffect
  ( windowModifierEffect
  , WindowModifierEffect(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Effect.Attrs

newtype WindowModifierEffect = WindowModifierEffect EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windowModifierEffect
  :: EffectId
  -> EffectMetadata Message
  -> EffectWindow
  -> Source
  -> Target
  -> WindowModifierEffect
windowModifierEffect eid metadata effectWindow source target =
  WindowModifierEffect $ EffectAttrs
    { effectId = eid
    , effectSource = source
    , effectTarget = target
    , effectCardCode = Nothing
    , effectMetadata = Just metadata
    , effectTraits = mempty
    , effectWindow = Just effectWindow
    }

instance HasModifiersFor env WindowModifierEffect where
  getModifiersFor _ target (WindowModifierEffect EffectAttrs {..})
    | target == effectTarget = case effectMetadata of
      Just (EffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env WindowModifierEffect where
  runMessage msg (WindowModifierEffect attrs) =
    WindowModifierEffect <$> runMessage msg attrs
