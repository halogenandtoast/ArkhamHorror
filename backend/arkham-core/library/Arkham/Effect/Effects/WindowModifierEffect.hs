module Arkham.Effect.Effects.WindowModifierEffect (
  windowModifierEffect,
  windowModifierEffect',
  WindowModifierEffect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Id
import Arkham.Matcher
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
      , effectSkillTest = mSkillTest
      }
 where
  mSkillTest = case (metadata, source, target) of
    (EffectMetaTarget (SkillTestTarget sid), _, _) -> Just sid
    (_, SkillTestSource sid, _) -> Just sid
    (_, _, SkillTestTarget sid) -> Just sid
    _ -> Nothing

instance HasModifiersFor WindowModifierEffect where
  getModifiersFor target (WindowModifierEffect EffectAttrs {..})
    | target == effectTarget = case effectMetadata of
        Just (EffectModifiers modifiers) -> case effectWindow of
          Just EffectSetupWindow -> pure $ map setActiveDuringSetup modifiers
          Just (EffectSkillTestWindow sid) -> do
            msid <- getSkillTestId
            pure $ guard (msid == Just sid) *> modifiers
          Just (EffectPhaseWindowFor p) -> do
            p' <- getPhase
            pure $ guard (p == p') *> modifiers
          Just (EffectTurnWindow iid) -> do
            isTurn <- iid <=~> TurnInvestigator
            pure $ guard isTurn *> modifiers
          _ -> pure modifiers
        _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage WindowModifierEffect where
  runMessage msg (WindowModifierEffect attrs) =
    WindowModifierEffect <$> runMessage msg attrs
