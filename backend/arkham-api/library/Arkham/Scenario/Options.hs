module Arkham.Scenario.Options where

import Arkham.Prelude
import Arkham.Id
import GHC.Records

data ScenarioOptions = ScenarioOptions
  { scenarioOptionsStandalone :: Bool
  , scenarioOptionsPerformTarotReading :: Bool
  , scenarioOptionsLeadInvestigator :: Maybe InvestigatorId
  , scenarioOptionsDelayChoosingLead :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass ToJSON

instance HasField "standalone" ScenarioOptions Bool where
  getField = scenarioOptionsStandalone

instance HasField "performTarotReading" ScenarioOptions Bool where
  getField = scenarioOptionsPerformTarotReading

instance HasField "leadInvestigator" ScenarioOptions (Maybe InvestigatorId) where
  getField = scenarioOptionsLeadInvestigator

instance HasField "delayChoosingLead" ScenarioOptions Bool where
  getField = scenarioOptionsDelayChoosingLead

defaultScenarioOptions :: ScenarioOptions
defaultScenarioOptions = ScenarioOptions
  { scenarioOptionsStandalone = False
  , scenarioOptionsPerformTarotReading = False
  , scenarioOptionsLeadInvestigator = Nothing
  , scenarioOptionsDelayChoosingLead = False
  }

instance FromJSON ScenarioOptions where
  parseJSON = withObject "ScenarioOptions" $ \o -> do
    scenarioOptionsStandalone <- o .: "scenarioOptionsStandalone"
    scenarioOptionsPerformTarotReading <- o .: "scenarioOptionsPerformTarotReading"
    scenarioOptionsLeadInvestigator <- o .:? "scenarioOptionsLeadInvestigator"
    scenarioOptionsDelayChoosingLead <- o .:? "scenarioOptionsDelayChoosingLead" .!= False
    pure ScenarioOptions {..}
