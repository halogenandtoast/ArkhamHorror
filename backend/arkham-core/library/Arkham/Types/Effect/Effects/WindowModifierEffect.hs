module Arkham.Types.Effect.Effects.WindowModifierEffect
  ( windowModifierEffect
  , WindowModifierEffect(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype WindowModifierEffect = WindowModifierEffect EffectAttrs
  deriving anyclass HasAbilities
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
      Just (FailedByEffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env WindowModifierEffect where
  runMessage msg e@(WindowModifierEffect attrs) = case msg of
    CancelFailedByModifierEffects -> case effectMetadata attrs of
      Just (FailedByEffectModifiers _) ->
        e <$ push (DisableEffect $ toId attrs)
      _ -> pure e
    _ -> WindowModifierEffect <$> runMessage msg attrs
