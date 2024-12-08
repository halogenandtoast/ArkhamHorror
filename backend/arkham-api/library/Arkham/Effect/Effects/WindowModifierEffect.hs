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
import Control.Monad.Writer.Class

newtype WindowModifierEffect = WindowModifierEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windowModifierEffect :: EffectArgs -> WindowModifierEffect
windowModifierEffect = WindowModifierEffect . uncurry (baseAttrs "wmode")

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
      , effectCardId = Nothing
      }
 where
  mSkillTest = case (metadata, source, target, effectWindow) of
    (EffectMetaTarget (SkillTestTarget sid), _, _, _) -> Just sid
    (_, SkillTestSource sid, _, _) -> Just sid
    (_, _, SkillTestTarget sid, _) -> Just sid
    (_, _, _, EffectSkillTestWindow sid) -> Just sid
    _ -> Nothing

instance HasModifiersFor WindowModifierEffect where
  getModifiersFor (WindowModifierEffect attrs) = case effectMetadata attrs of
    Just (EffectModifiers modifiers) -> case effectWindow attrs of
      Just EffectSetupWindow -> tell $ singletonMap attrs.target $ map setActiveDuringSetup modifiers
      Just (EffectSkillTestWindow sid) -> do
        msid <- getSkillTestId
        when (msid == Just sid) $ tell $ singletonMap attrs.target modifiers
      Just (EffectPhaseWindowFor p) -> do
        p' <- getPhase
        when (p == p') $ tell $ singletonMap attrs.target modifiers
      Just (EffectTurnWindow iid) -> do
        isTurn <- iid <=~> TurnInvestigator
        when isTurn $ tell $ singletonMap attrs.target modifiers
      _ -> tell $ singletonMap attrs.target modifiers
    _ -> pure ()

instance RunMessage WindowModifierEffect where
  runMessage msg (WindowModifierEffect attrs) =
    WindowModifierEffect <$> runMessage msg attrs
