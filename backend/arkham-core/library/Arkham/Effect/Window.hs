module Arkham.Effect.Window (
  module Arkham.Effect.Window,
) where

import Arkham.Prelude

import Arkham.Card.Id
import Arkham.Id
import Arkham.Phase
import GHC.OverloadedLabels

data EffectWindow
  = EffectPhaseWindow
  | EffectPhaseWindowFor Phase
  | EffectUntilEndOfPhaseWindowFor Phase
  | EffectCostWindow
  | EffectSkillTestWindow
  | EffectRoundWindow
  | EffectNextActionWindow
  | EffectSetupWindow
  | EffectTurnWindow InvestigatorId
  | EffectNextTurnWindow InvestigatorId
  | EffectCardResolutionWindow CardId
  | EffectGameWindow
  | EffectAttackWindow
  | FirstEffectWindow [EffectWindow]
  | EffectEventWindow
  | EffectAbilityWindow
  | EffectGainResourcesWindow InvestigatorId
  | EffectSearchWindow
  | EffectCardCostWindow CardId
  | EffectCardDrawWindow
  | EffectUI
  | EffectMoveWindow
  | EffectRevelationWindow TreacheryId
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsLabel "skillTest" EffectWindow where
  fromLabel = EffectSkillTestWindow
