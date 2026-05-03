{-# LANGUAGE TemplateHaskell #-}

module Arkham.Effect.Window (module Arkham.Effect.Window) where

import Arkham.ScenarioLogKey
import Arkham.Ability.Types
import Arkham.Card.Id
import Arkham.Id
import Arkham.Matcher.SkillTest
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
  | EffectSkillTestMatchingWindow SkillTestMatcher
  | EffectNextSkillTestWindow InvestigatorId
  | EffectRoundWindow
  | EffectNextActionWindow
  | EffectActWindow
  | EffectSetupWindow
  | EffectScenarioSetupWindow ScenarioId
  | EffectTurnWindow InvestigatorId
  | EffectEndOfNextTurnWindow InvestigatorId
  | EffectNextTurnWindow InvestigatorId
  | EffectCardResolutionWindow CardId
  | EffectGameWindow
  | EffectResolutionWindow
  | EffectAttackWindow
  | EffectDamageWindow
  | FirstEffectWindow [EffectWindow]
  | EffectEventWindow
  | EffectAbilityWindow AbilityRef
  | EffectGainResourcesWindow InvestigatorId
  | EffectSearchWindow
  | EffectCardCostWindow CardId
  | EffectCardDrawWindow CardDrawId
  | EffectUI
  | EffectMoveWindow
  | EffectThisMoveWindow MovementId
  | EffectRevelationWindow TreacheryId
  | EffectDefeatWindow EnemyId
  | EffectHollowWindow CardId
  | EffectRememberedWindow ScenarioLogKey
  deriving stock (Ord, Eq, Show, Data)

instance IsLabel "endOfCurrentPhase" EffectWindow where
  fromLabel = EffectPhaseWindow

instance IsLabel "endOfNextSkillTest" (InvestigatorId -> EffectWindow) where
  fromLabel = EffectNextSkillTestWindow

instance IsLabel "nextSkillTest" (InvestigatorId -> EffectWindow) where
  fromLabel = EffectNextSkillTestWindow

instance IsLabel "remembered" (ScenarioLogKey -> EffectWindow) where
  fromLabel = EffectRememberedWindow

instance IsLabel "round" EffectWindow where
  fromLabel = EffectRoundWindow

instance IsLabel "nextAction" EffectWindow where
  fromLabel = EffectNextActionWindow

instance IsLabel "resolution" EffectWindow where
  fromLabel = EffectResolutionWindow

instance IsLabel "move" EffectWindow where
  fromLabel = EffectMoveWindow

instance IsLabel "turn" (InvestigatorId -> EffectWindow) where
  fromLabel = EffectTurnWindow

instance IsLabel "skillTest" (SkillTestId -> EffectWindow) where
  fromLabel = EffectSkillTestWindow

instance IsLabel "skillTestMatching" (SkillTestMatcher -> EffectWindow) where
  fromLabel = EffectSkillTestMatchingWindow

firstWindow :: [EffectWindow] -> EffectWindow
firstWindow = FirstEffectWindow

$(deriveToJSON defaultOptions ''EffectWindow)

instance FromJSON EffectWindow where
  parseJSON = withObject "EffectWindow" \o -> do
    t <- o .: "tag"
    case t :: Text of
      "EffectCardDrawWindow" -> do
        contents <- o .:? "contents" .!= CardDrawId UUID.nil
        pure $ EffectCardDrawWindow contents
      _ -> $(mkParseJSON defaultOptions ''EffectWindow) (Object o)
