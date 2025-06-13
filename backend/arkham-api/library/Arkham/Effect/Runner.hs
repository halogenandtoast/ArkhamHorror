{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Effect.Runner (intFromMetadata, module X) where

import Arkham.Prelude

import Arkham.Effect.Types as X
import Arkham.Effect.Window as X
import Arkham.EffectMetadata as X
import Arkham.Helpers.Message as X
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Id as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.Classes.Query (selectOne, (<=~>))
import Arkham.Classes.RunMessage
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher.Scenario
import Arkham.Modifier

intFromMetadata :: EffectMetadata a -> Int
intFromMetadata = \case
  EffectInt n -> n
  _ -> 0

instance RunMessage EffectAttrs where
  runMessage msg a@EffectAttrs {..} = case msg of
    DrawEnded _ | isEndOfWindow a EffectCardDrawWindow -> do
      a <$ push (DisableEffect effectId)
    SearchEnded _ | isEndOfWindow a EffectSearchWindow -> do
      a <$ push (DisableEffect effectId)
    EndSearch {} | isEndOfWindow a EffectSearchWindow -> do
      a <$ push (DisableEffect effectId)
    CancelSearch {} | isEndOfWindow a EffectSearchWindow -> do
      a <$ push (DisableEffect effectId)
    EndSetup | isEndOfWindow a EffectSetupWindow -> do
      a <$ push (DisableEffect effectId)
    EndSetup -> do
      selectOne TheScenario >>= traverse_ \scenarioId ->
        pushWhen (isEndOfWindow a (EffectScenarioSetupWindow scenarioId)) (DisableEffect effectId)
      pure a
    Begin p | isEndOfWindow a (EffectUntilEndOfNextPhaseWindowFor p) -> do
      pure $ a {effectWindow = Just $ EffectUntilEndOfPhaseWindowFor p}
    EndPhase | isEndOfWindow a EffectPhaseWindow -> do
      a <$ push (DisableEffect effectId)
    EndPhase -> do
      phase <- getPhase
      pushWhen
        ( isEndOfWindow a (EffectPhaseWindowFor phase)
            || isEndOfWindow a (EffectUntilEndOfPhaseWindowFor phase)
        )
        $ (DisableEffect effectId)
      pure a
    BeginTurn iid | isEndOfWindow a (EffectNextTurnWindow iid) -> do
      a <$ push (DisableEffect effectId)
    EndTurn iid | isEndOfWindow a (EffectTurnWindow iid) -> do
      a <$ push (DisableEffect effectId)
    EndRound | isEndOfWindow a EffectRoundWindow -> do
      a <$ push (DisableEffect effectId)
    FinishedEvent _ | isEndOfWindow a EffectEventWindow -> do
      a <$ push (DisableEffect effectId)
    BeginAction | isEndOfWindow a EffectNextActionWindow -> do
      a <$ push (DisableEffect effectId)
    ReplaceAct {} | isEndOfWindow a EffectActWindow -> do
      a <$ push (DisableEffect effectId)
    Discard _ _ (ActTarget _) | isEndOfWindow a EffectActWindow -> do
      a <$ push (DisableEffect effectId)
    SkillTestEnded sid | isEndOfWindow a (EffectSkillTestWindow sid) -> do
      push $ DisableEffect effectId
      pure a
    CancelSkillEffects -> case effectSource of
      (SkillSource _) -> a <$ push (DisableEffect effectId)
      _ -> pure a
    PaidAllCosts {} | isEndOfWindow a EffectCostWindow -> do
      a <$ push (DisableEffect effectId)
    PlayCard _ card _ _ _ _ | isEndOfWindow a (EffectCardCostWindow $ toCardId card) -> do
      a <$ push (DisableEffect effectId)
    After (PerformEnemyAttack {}) | isEndOfWindow a EffectAttackWindow -> do
      a <$ push (DisableEffect effectId)
    AfterRevelation _ tid | isEndOfWindow a (EffectRevelationWindow tid) -> do
      a <$ push (DisableEffect effectId)
    ResolvedCard _ card | isEndOfWindow a (EffectCardResolutionWindow $ toCardId card) -> do
      a <$ push (Priority $ DisableEffect effectId)
    ResolvedAbility ab | isEndOfWindow a (EffectAbilityWindow ab.ref) -> do
      a <$ push (DisableEffect effectId)
    Do (TakeResources iid _ _ _) | isEndOfWindow a (EffectGainResourcesWindow iid) -> do
      a <$ push (DisableEffect effectId)
    ClearUI | isEndOfWindow a EffectUI -> do
      a <$ push (DisableEffect effectId)
    MoveAction _ _ _ False | isEndOfWindow a EffectMoveWindow -> do
      a <$ push (DisableEffect effectId)
    Move _ | isEndOfWindow a EffectMoveWindow -> do
      a <$ push (DisableEffect effectId)
    WhenCanMove _ _ | isEndOfWindow a EffectMoveWindow -> do
      -- We've killed the entire batch at this point so we can resume
      a <$ push (DisableEffect effectId)
    ResolvedAbility ab | #move `elem` ab.actions && isEndOfWindow a EffectMoveWindow -> do
      a <$ push (DisableEffect effectId)
    NextSkillTest sid -> do
      getSkillTestInvestigator >>= \case
        Just iid -> pure $ replaceNextSkillTest sid iid a
        Nothing -> pure a
    RepeatSkillTest sid stId | isEndOfWindow a (EffectSkillTestWindow stId) -> do
      getSkillTest >>= \case
        Nothing -> pure a
        Just st ->
          if st.id == stId && st.source == a.source
            then pure $ a {effectWindow = Just $ EffectSkillTestWindow sid}
            else pure a
    UpdateEffectMeta eid meta | eid == effectId -> do
      pure $ a {effectMetadata = Just meta}
    CommitCard iid card -> do
      case a.metadata of
        (Just (EffectModifiers modifiers)) -> do
          let
            updateModifier = \case
              x@(modifierType -> OnCommitCardModifier iid' matcher m) | iid == iid' -> do
                ok <- card <=~> matcher
                pure $ if ok then x {modifierType = m} else x
              other -> pure other
          modifiers' <- traverse updateModifier modifiers
          pure $ a {effectMetadata = Just $ EffectModifiers modifiers'}
        _ -> pure a
    UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg
      pure a
    InSearch msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    InDiscard _ msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    InHand _ msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    _ -> pure a
