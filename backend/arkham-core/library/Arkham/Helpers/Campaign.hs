module Arkham.Helpers.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.Classes.Query
import Arkham.GameEnv
import Arkham.Game ()
import Arkham.Matcher
import Arkham.Projection
import Arkham.Id
import Arkham.CampaignStep

getCompletedScenarios :: (Monad m, HasGame m) => m (HashSet ScenarioId)
getCompletedScenarios = do
  mcampaignId <- selectOne TheCampaign
  case mcampaignId of
    Nothing -> pure mempty
    Just campaignId -> do
      completedSteps <- field CampaignCompletedSteps campaignId
      pure . setFromList $ flip mapMaybe completedSteps $ \case
        ScenarioStep scenarioId -> Just scenarioId
        _ -> Nothing
