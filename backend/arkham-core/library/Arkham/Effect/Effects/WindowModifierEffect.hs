module Arkham.Effect.Effects.WindowModifierEffect (
  windowModifierEffect,
  windowModifierEffect',
  WindowModifierEffect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Window (Window)

newtype WindowModifierEffect = WindowModifierEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

windowModifierEffect :: EffectArgs -> WindowModifierEffect
windowModifierEffect = WindowModifierEffect . uncurry4 (baseAttrs "wmode")

windowModifierEffect'
  :: EffectId
  -> EffectMetadata Window Message
  -> EffectWindow
  -> Source
  -> Target
  -> WindowModifierEffect
windowModifierEffect' eid metadata effectWindow source target =
  WindowModifierEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = target
      , effectCardCode = "wmode"
      , effectMetadata = Just metadata
      , effectTraits = mempty
      , effectWindow = Just effectWindow
      , effectFinished = False
      , effectExtraMetadata = Null
      }

instance HasModifiersFor WindowModifierEffect where
  getModifiersFor target (WindowModifierEffect EffectAttrs {..})
    | target == effectTarget = case effectMetadata of
        Just (EffectModifiers modifiers) -> case effectWindow of
          Just EffectSetupWindow -> pure $ map setActiveDuringSetup modifiers
          _ -> pure modifiers
        Just (FailedByEffectModifiers modifiers) -> pure modifiers
        _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage WindowModifierEffect where
  runMessage msg e@(WindowModifierEffect attrs) = case msg of
    CancelFailedByModifierEffects -> case effectMetadata attrs of
      Just (FailedByEffectModifiers _) -> do
        push (DisableEffect $ toId attrs)
        pure e
      _ -> pure e
    _ -> WindowModifierEffect <$> runMessage msg attrs
