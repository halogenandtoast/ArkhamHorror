{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Effect.Runner (intFromMetadata, module X) where

import Arkham.Prelude

import Arkham.Effect.Types as X
import Arkham.Effect.Window as X
import Arkham.EffectMetadata as X
import Arkham.Helpers.Message as X
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Card
import Arkham.Classes.RunMessage
import {-# SOURCE #-} Arkham.GameEnv

intFromMetadata :: EffectMetadata window a -> Int
intFromMetadata = \case
  EffectInt n -> n
  _ -> 0

instance RunMessage EffectAttrs where
  runMessage msg a@EffectAttrs {..} = case msg of
    DrawEnded _ | isEndOfWindow a EffectCardDrawWindow -> do
      a <$ push (DisableEffect effectId)
    SearchEnded _ | isEndOfWindow a EffectSearchWindow -> do
      a <$ push (DisableEffect effectId)
    CancelSearch {} | isEndOfWindow a EffectSearchWindow -> do
      a <$ push (DisableEffect effectId)
    EndSetup | isEndOfWindow a EffectSetupWindow -> do
      a <$ push (DisableEffect effectId)
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
    SkillTestEnded sid | isEndOfWindow a (EffectSkillTestWindow sid) || isEndOfWindow a EffectNextSkillTestWindow -> do
      a <$ push (DisableEffect effectId)
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
      a <$ push (DisableEffect effectId)
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
    NextSkillTest sid -> pure $ replaceNextSkillTest sid a
    _ -> pure a
