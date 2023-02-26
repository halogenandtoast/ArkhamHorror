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

getCampaignLog :: HasGame m => m CampaignLog
getCampaignLog = withStandalone
  (field CampaignCampaignLog)
  (field ScenarioStandaloneCampaignLog)

getHasRecord :: HasGame m => CampaignLogKey -> m Bool
getHasRecord k = do
  campaignLog <- getCampaignLog
  pure $ or
    [ k `member` campaignLogRecorded campaignLog
    , k `member` campaignLogRecordedCounts campaignLog
    ]

whenHasRecord :: HasGame m => CampaignLogKey -> m () -> m ()
whenHasRecord k = whenM (getHasRecord k)

getRecordCount :: HasGame m => CampaignLogKey -> m Int
getRecordCount k =
  findWithDefault 0 k . campaignLogRecordedCounts <$> getCampaignLog

getRecordSet :: HasGame m => CampaignLogKey -> m [Recorded CardCode]
getRecordSet k =
  findWithDefault [] k . campaignLogRecordedSets <$> getCampaignLog

getRecordedCardCodes :: HasGame m => CampaignLogKey -> m [CardCode]
getRecordedCardCodes k = mapMaybe onlyRecorded <$> getRecordSet k
 where
  onlyRecorded = \case
    Recorded cCode -> Just cCode
    CrossedOut _ -> Nothing

remembered :: HasGame m => ScenarioLogKey -> m Bool
remembered k = member k <$> scenarioField ScenarioRemembered

scenarioCount :: HasGame m => ScenarioCountKey -> m Int
scenarioCount k = fromMaybe 0 . lookup k <$> scenarioField ScenarioCounts
