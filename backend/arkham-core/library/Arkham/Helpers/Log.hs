module Arkham.Helpers.Log where

import Arkham.Prelude

import Arkham.Campaign.Types ( Field (..) )
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Arkham.ScenarioLogKey

getCampaignLog :: (Monad m, HasGame m) => m CampaignLog
getCampaignLog = withStandalone
  (field CampaignCampaignLog)
  (field ScenarioStandaloneCampaignLog)

getHasRecord :: (Monad m, HasGame m) => CampaignLogKey -> m Bool
getHasRecord k = do
  campaignLog <- getCampaignLog
  pure $ k `member` (campaignLogRecorded campaignLog)

getRecordCount :: (Monad m, HasGame m) => CampaignLogKey -> m Int
getRecordCount k = do
  campaignLog <- getCampaignLog
  pure $ findWithDefault 0 k (campaignLogRecordedCounts campaignLog)

getRecordSet :: (Monad m, HasGame m) => CampaignLogKey -> m [Recorded CardCode]
getRecordSet k = do
  campaignLog <- getCampaignLog
  pure $ findWithDefault [] k (campaignLogRecordedSets campaignLog)

remembered :: (Monad m, HasGame m) => ScenarioLogKey -> m Bool
remembered k = member k <$> scenarioField ScenarioRemembered
