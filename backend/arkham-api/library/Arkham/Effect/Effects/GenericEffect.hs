module Arkham.Effect.Effects.GenericEffect (
  genericEffect,
  GenericEffect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Criteria
import Arkham.Effect.Runner
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Matcher
import Arkham.Modifier
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict (MonoidalMap (..))

newtype GenericEffect = GenericEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

genericEffect :: EffectArgs -> GenericEffect
genericEffect = GenericEffect . uncurry (baseAttrs "genef")

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
      Just (EffectSkillTestMatchingWindow stmatch) -> do
        ms <- getSkillTest
        for_ ms \s -> do
          whenM (skillTestMatches s.investigator s.source s stmatch) do
            tell $ MonoidalMap $ singletonMap attrs.target modifiers
      Just (EffectPhaseWindowFor p) -> do
        p' <- getPhase
        modifiers' <- resolveModifiers modifiers
        when (p == p') $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
      Just (EffectTurnWindow iid) -> do
        isTurn <- iid <=~> TurnInvestigator
        modifiers' <- resolveModifiers modifiers
        when isTurn $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
      Just (EffectNextSkillTestWindow {}) -> pure ()
      Just (FirstEffectWindow ws) -> case firstEffectWindowGate ws of
        FirstWindowSuppress -> pure ()
        FirstWindowSkillTest sid -> do
          msid <- getSkillTestId
          modifiers' <- resolveModifiers modifiers
          when (msid == Just sid) $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
        FirstWindowTurn iid -> do
          isTurn <- iid <=~> TurnInvestigator
          modifiers' <- resolveModifiers modifiers
          when isTurn $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
        FirstWindowPhase p -> do
          p' <- getPhase
          modifiers' <- resolveModifiers modifiers
          when (p == p') $ tell $ MonoidalMap $ singletonMap attrs.target modifiers'
        FirstWindowEmit -> do
          modifiers' <- resolveModifiers modifiers
          tell $ MonoidalMap $ singletonMap attrs.target modifiers'
      _ -> do
        modifiers' <- resolveModifiers modifiers
        tell $ MonoidalMap $ singletonMap attrs.target modifiers'
    _ -> pure ()
   where
    resolveModifiers [] = pure []
    resolveModifiers (x@(modifierType -> (CriteriaModifier (EnemyCriteria (EnemyExists matcher)) m)) : ms) = do
      valid <- selectAny matcher
      if valid then (x {modifierType = m} :) <$> resolveModifiers ms else resolveModifiers ms
    resolveModifiers (m : ms) = (m :) <$> resolveModifiers ms

instance RunMessage GenericEffect where
  runMessage msg (GenericEffect attrs) =
    GenericEffect <$> runMessage msg attrs
