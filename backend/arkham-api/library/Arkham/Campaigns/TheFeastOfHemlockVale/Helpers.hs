{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Campaigns.TheFeastOfHemlockVale.Helpers where

import Arkham.CampaignStep
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Criteria
import Arkham.Helpers.Campaign
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher.Scenario
import Arkham.Message (Message (NextCampaignStep))
import Arkham.Message.Lifted hiding (continue)
import Arkham.Modifier
import Arkham.Prelude hiding (Day)
import Arkham.Scenario.Options
import Arkham.Source
import Arkham.Target
import Arkham.Tracing

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theFeastOfHemlockVale" a

codex :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
codex iid (toSource -> source) n = scenarioSpecific "codex" (iid, source, n)

data Day = Day1 | Day2 | Day3
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Time = Night | Day
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

initMeta :: TheFeastOfHemlockValeMeta
initMeta = TheFeastOfHemlockValeMeta Day1 Day

data TheFeastOfHemlockValeMeta = TheFeastOfHemlockValeMeta
  { day :: Day
  , time :: Time
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

getCampaignTime :: (Tracing m, HasGame m) => m Time
getCampaignTime = withCampaignMeta @TheFeastOfHemlockValeMeta (.time)

getCampaignDay :: (Tracing m, HasGame m) => m Day
getCampaignDay = withCampaignMeta @TheFeastOfHemlockValeMeta (.day)

pattern IsDay :: Criterion
pattern IsDay <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Day")))
  where
    IsDay = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Day")))

pattern IsNight :: Criterion
pattern IsNight <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Night")))
  where
    IsNight = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Night")))

setScenarioDayAndTime :: ReverseQueue m => m ()
setScenarioDayAndTime = do
  day <- getCampaignDay
  time <- getCampaignTime
  gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "day" (toJSON day))
  gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "time" (toJSON time))

afterPrelude :: ReverseQueue m => CampaignStep -> m ()
afterPrelude =
  setNextCampaignStep . \case
    ScenarioStep sid ->
      ScenarioStepWithOptions sid defaultScenarioOptions {scenarioOptionsSkipInvestigatorSetup = True}
    other -> other
 where
  setNextCampaignStep = push . NextCampaignStep . continueNoUpgrade

pattern Theta :: Int
pattern Theta = 100

pattern Omega :: Int
pattern Omega = 101

pattern Psi :: Int
pattern Psi = 102

pattern Phi :: Int
pattern Phi = 103

pattern Sigma :: Int
pattern Sigma = 104
