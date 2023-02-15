{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Effect.Runner (module X) where

import Arkham.Prelude

import Arkham.Effect.Types as X
import Arkham.Effect.Window as X
import Arkham.EffectMetadata as X
import Arkham.Helpers.SkillTest as X

import Arkham.Classes.RunMessage
import Arkham.Classes.HasQueue
import Arkham.Source
import Arkham.Message

instance RunMessage EffectAttrs where
  runMessage msg a@EffectAttrs {..} = case msg of
    EndSetup | isEndOfWindow a EffectSetupWindow ->
      a <$ push (DisableEffect effectId)
    EndPhase | isEndOfWindow a EffectPhaseWindow ->
      a <$ push (DisableEffect effectId)
    EndTurn _ | isEndOfWindow a EffectTurnWindow ->
      a <$ push (DisableEffect effectId)
    EndRound | isEndOfWindow a EffectRoundWindow ->
      a <$ push (DisableEffect effectId)
    SkillTestEnds _ _ | isEndOfWindow a EffectSkillTestWindow ->
      a <$ push (DisableEffect effectId)
    CancelSkillEffects -> case effectSource of
      (SkillSource _) -> a <$ push (DisableEffect effectId)
      _ -> pure a
    PaidCost _ _ _ _ | isEndOfWindow a EffectCostWindow ->
      a <$ push (DisableEffect effectId)
    After (PerformEnemyAttack {}) | isEndOfWindow a EffectAttackWindow ->
      a <$ push (DisableEffect effectId)
    _ -> pure a

