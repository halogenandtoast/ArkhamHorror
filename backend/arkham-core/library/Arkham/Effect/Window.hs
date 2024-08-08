{-# LANGUAGE TemplateHaskell #-}

module Arkham.Effect.Window (
  module Arkham.Effect.Window,
) where

import Arkham.Ability.Types
import Arkham.Card.Id
import Arkham.Id
import Arkham.Phase
import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels

data EffectWindow
  = EffectPhaseWindow
  | EffectPhaseWindowFor Phase
  | EffectUntilEndOfPhaseWindowFor Phase
  | EffectUntilEndOfNextPhaseWindowFor Phase
  | EffectCostWindow
  | EffectSkillTestWindow SkillTestId
  | EffectNextSkillTestWindow
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
  | EffectAbilityWindow AbilityRef
  | EffectGainResourcesWindow InvestigatorId
  | EffectSearchWindow
  | EffectCardCostWindow CardId
  | EffectCardDrawWindow
  | EffectUI
  | EffectMoveWindow
  | EffectRevelationWindow TreacheryId
  deriving stock (Eq, Show, Data)

instance IsLabel "round" EffectWindow where
  fromLabel = EffectRoundWindow

instance IsLabel "skillTest" (SkillTestId -> EffectWindow) where
  fromLabel = EffectSkillTestWindow

$(deriveJSON defaultOptions ''EffectWindow)
