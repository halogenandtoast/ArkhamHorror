{-# LANGUAGE TemplateHaskell #-}

module Arkham.Effect.Window (module Arkham.Effect.Window) where

import Arkham.Ability.Types
import Arkham.Card.Id
import Arkham.Id
import Arkham.Phase
import Arkham.Prelude
import Data.Aeson.TH
import Data.UUID qualified as UUID
import GHC.OverloadedLabels

data EffectWindow
  = EffectPhaseWindow
  | EffectPhaseWindowFor Phase
  | EffectUntilEndOfPhaseWindowFor Phase
  | EffectUntilEndOfNextPhaseWindowFor Phase
  | EffectCostWindow
  | EffectSkillTestWindow SkillTestId
  | EffectNextSkillTestWindow InvestigatorId
  | EffectRoundWindow
  | EffectNextActionWindow
  | EffectActWindow
  | EffectSetupWindow
  | EffectScenarioSetupWindow ScenarioId
  | EffectTurnWindow InvestigatorId
  | EffectNextTurnWindow InvestigatorId
  | EffectCardResolutionWindow CardId
  | EffectGameWindow
  | EffectResolutionWindow
  | EffectAttackWindow
  | FirstEffectWindow [EffectWindow]
  | EffectEventWindow
  | EffectAbilityWindow AbilityRef
  | EffectGainResourcesWindow InvestigatorId
  | EffectSearchWindow
  | EffectCardCostWindow CardId
  | EffectCardDrawWindow CardDrawId
  | EffectUI
  | EffectMoveWindow
  | EffectRevelationWindow TreacheryId
  deriving stock (Ord, Eq, Show, Data)

instance IsLabel "endOfCurrentPhase" EffectWindow where
  fromLabel = EffectPhaseWindow

instance IsLabel "endOfNextSkillTest" (InvestigatorId -> EffectWindow) where
  fromLabel = EffectNextSkillTestWindow

instance IsLabel "nextSkillTest" (InvestigatorId -> EffectWindow) where
  fromLabel = EffectNextSkillTestWindow

instance IsLabel "round" EffectWindow where
  fromLabel = EffectRoundWindow

instance IsLabel "resolution" EffectWindow where
  fromLabel = EffectResolutionWindow

instance IsLabel "move" EffectWindow where
  fromLabel = EffectMoveWindow

instance IsLabel "skillTest" (SkillTestId -> EffectWindow) where
  fromLabel = EffectSkillTestWindow

$(deriveToJSON defaultOptions ''EffectWindow)

instance FromJSON EffectWindow where
  parseJSON = withObject "EffectWindow" \o -> do
    t <- o .: "tag"
    case t :: Text of
      "EffectCardDrawWindow" -> do
        contents <- o .:? "contents" .!= CardDrawId UUID.nil
        pure $ EffectCardDrawWindow contents
      _ -> $(mkParseJSON defaultOptions ''EffectWindow) (Object o)
