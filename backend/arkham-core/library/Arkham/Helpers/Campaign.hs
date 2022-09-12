module Arkham.Helpers.Campaign where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection

completedScenario :: (Monad m, HasGame m) => ScenarioId -> m Bool
completedScenario cCode = elem cCode <$> getCompletedScenarios

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
