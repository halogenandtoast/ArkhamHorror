module Arkham.Effect.Window (
  module Arkham.Effect.Window,
) where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Card.Id
import Arkham.Id
import Arkham.Phase
import Data.UUID (nil)
import GHC.OverloadedLabels

data EffectWindow
  = EffectPhaseWindow
  | EffectPhaseWindowFor Phase
  | EffectUntilEndOfPhaseWindowFor Phase
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
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass ToJSON

instance FromJSON EffectWindow where
  parseJSON = withObject "EffectWindow" $ \o -> do
    tag :: String <- o .: "tag"
    case tag of
      "EffectSkillTestWindow" -> do
        skillTestId <- o .:? "contents" .!= SkillTestId nil
        pure $ EffectSkillTestWindow skillTestId
      _ -> genericParseJSON defaultOptions (Object o)

instance IsLabel "skillTest" (SkillTestId -> EffectWindow) where
  fromLabel = EffectSkillTestWindow
