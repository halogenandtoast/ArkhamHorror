module Arkham.Effect.Effects.GenericEffect (
  genericEffect,
  genericEffect',
  GenericEffect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Criteria
import Arkham.Effect.Runner
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (Window)
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict (MonoidalMap (..))

newtype GenericEffect = GenericEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

genericEffect :: EffectArgs -> GenericEffect
genericEffect = GenericEffect . uncurry (baseAttrs "genef")

genericEffect'
  :: EffectId
  -> EffectMetadata Window Message
  -> EffectWindow
  -> Source
  -> Target
  -> GenericEffect
genericEffect' eid metadata effectWindow source target =
  GenericEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = target
      , effectCardCode = "genef"
      , effectMetadata = Just metadata
      , effectTraits = mempty
      , effectWindow = Just effectWindow
      , effectDisableWindow = Nothing
      , effectOnDisable = Nothing
      , effectFinished = False
      , effectExtraMetadata = Null
      , effectSkillTest = mSkillTest
      , effectCardId = Nothing
      , effectMetaKeys = []
      }
 where
  mSkillTest = case (metadata, source, target, effectWindow) of
    (EffectMetaTarget (SkillTestTarget sid), _, _, _) -> Just sid
    (_, SkillTestSource sid, _, _) -> Just sid
    (_, _, SkillTestTarget sid, _) -> Just sid
    (_, _, _, EffectSkillTestWindow sid) -> Just sid
    _ -> Nothing

instance HasModifiersFor GenericEffect where
  getModifiersFor (GenericEffect attrs) = case effectMetadata attrs of
    Just (EffectModifiers modifiers) -> case effectWindow attrs of
      Just EffectSetupWindow -> do
        modifiers' <- resolveModifiers modifiers
        tell $ MonoidalMap $ singletonMap attrs.target $ map setActiveDuringSetup modifiers'
      Just (EffectScenarioSetupWindow scenarioId) -> do
        selectOne TheScenario >>= traverse_ \currentScenarioId ->
          when (scenarioId == currentScenarioId) do
            modifiers' <- resolveModifiers modifiers
            tell $ MonoidalMap $ singletonMap attrs.target $ map setActiveDuringSetup modifiers'
      Just (EffectSkillTestWindow sid) -> do
        msid <- getSkillTestId
        modifiers' <- resolveModifiers modifiers
        when (msid == Just sid) $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
      Just (EffectPhaseWindowFor p) -> do
        p' <- getPhase
        modifiers' <- resolveModifiers modifiers
        when (p == p') $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
      Just (EffectTurnWindow iid) -> do
        isTurn <- iid <=~> TurnInvestigator
        modifiers' <- resolveModifiers modifiers
        when isTurn $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
      _ -> do
        modifiers' <- resolveModifiers modifiers
        tell $ MonoidalMap $ singletonMap attrs.target modifiers'
    _ -> pure ()
   where
    resolveModifiers [] = pure []
    resolveModifiers (x@(modifierType -> (CriteriaModifier (EnemyCriteria (EnemyExists matcher)) m)) : ms) = do
      valid <- selectAny matcher
      if valid then (x { modifierType = m } :) <$> resolveModifiers ms else resolveModifiers ms
    resolveModifiers (m : ms) = (m:) <$> resolveModifiers ms

instance RunMessage GenericEffect where
  runMessage msg (GenericEffect attrs) =
    GenericEffect <$> runMessage msg attrs
