module Arkham.Effect.Window (
  module Arkham.Effect.Window,
) where

import Arkham.Prelude

import Arkham.Card.Id
import Arkham.Phase
import GHC.OverloadedLabels

data EffectWindow
  = EffectPhaseWindow
  | EffectPhaseWindowFor Phase
  | EffectCostWindow
  | EffectSkillTestWindow
  | EffectRoundWindow
  | EffectNextActionWindow
  | EffectSetupWindow
  | EffectTurnWindow
  | EffectNextTurnWindow
  | EffectCardResolutionWindow CardId
  | EffectGameWindow
  | EffectAttackWindow
  | FirstEffectWindow [EffectWindow]
  | EffectEventWindow
  | EffectAbilityWindow
  | EffectSearchWindow
  | EffectCardCostWindow CardId
  | EffectCardDrawWindow
  | EffectUI
  | EffectMoveWindow
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsLabel "skillTest" EffectWindow where
  fromLabel = EffectSkillTestWindow
