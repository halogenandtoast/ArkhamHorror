module Arkham.Types.CampaignLog where

import Arkham.Json
import ClassyPrelude
import Lens.Micro

data CampaignLogKey
  = GhoulPriestIsStillAlive
  | YourHouseIsStillStanding
  | YourHouseHasBurnedToTheGround
  | LitaWasForcedToFindOthersToHelpHerCause
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)

data CampaignLog = CampaignLog
  { campaignLogRecorded :: HashSet CampaignLogKey
  , campaignLogRecordedCounts :: HashMap CampaignLogKey Int
  }
  deriving stock (Show, Generic)

recorded :: Lens' CampaignLog (HashSet CampaignLogKey)
recorded = lens campaignLogRecorded $ \m x -> m { campaignLogRecorded = x }

recordedCounts :: Lens' CampaignLog (HashMap CampaignLogKey Int)
recordedCounts = lens campaignLogRecordedCounts $ \m x -> m { campaignLogRecordedCounts = x }

mkCampaignLog :: CampaignLog
mkCampaignLog =
  CampaignLog { campaignLogRecorded = mempty, campaignLogRecordedCounts = mempty }

instance ToJSON CampaignLog where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignLog"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignLog"

instance FromJSON CampaignLog where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignLog"
