module Arkham.Effect.Effects.WindowModifierEffect (
  windowModifierEffect,
  windowModifierEffect',
  WindowModifierEffect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import {-# SOURCE #-} Arkham.GameEnv (getIsSkillTest)
import Arkham.Id
import Arkham.Modifier
import Arkham.Window (Window)

newtype WindowModifierEffect = WindowModifierEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
          Just EffectSkillTestWindow -> do
            isSkillTest <- getIsSkillTest
            pure $ guard isSkillTest *> modifiers
          _ -> pure modifiers
        _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage WindowModifierEffect where
  runMessage msg (WindowModifierEffect attrs) =
    WindowModifierEffect <$> runMessage msg attrs
