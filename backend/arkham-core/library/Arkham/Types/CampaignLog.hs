module Arkham.Types.CampaignLog where

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes.HasRecord
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

data CampaignLog = CampaignLog
  { campaignLogRecorded :: HashSet CampaignLogKey
  , campaignLogRecordedCounts :: HashMap CampaignLogKey Int
  }
  deriving stock (Show, Generic)

instance HasRecord CampaignLog where
  hasRecord key = HashSet.member key . campaignLogRecorded

recorded :: Lens' CampaignLog (HashSet CampaignLogKey)
recorded = lens campaignLogRecorded $ \m x -> m { campaignLogRecorded = x }

recordedCounts :: Lens' CampaignLog (HashMap CampaignLogKey Int)
recordedCounts =
  lens campaignLogRecordedCounts $ \m x -> m { campaignLogRecordedCounts = x }

mkCampaignLog :: CampaignLog
mkCampaignLog = CampaignLog
  { campaignLogRecorded = mempty
  , campaignLogRecordedCounts = mempty
  }

instance ToJSON CampaignLog where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignLog"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignLog"

instance FromJSON CampaignLog where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignLog"
