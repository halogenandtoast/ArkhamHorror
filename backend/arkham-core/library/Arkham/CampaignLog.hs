module Arkham.CampaignLog where

import Arkham.Prelude

import Arkham.Json
import Arkham.CampaignLogKey
import Arkham.Card.CardCode

data CampaignLog = CampaignLog
  { campaignLogRecorded :: HashSet CampaignLogKey
  , campaignLogCrossedOut :: HashSet CampaignLogKey
  , campaignLogRecordedCounts :: HashMap CampaignLogKey Int
  , campaignLogRecordedSets :: HashMap CampaignLogKey [Recorded CardCode]
  }
  deriving stock (Show, Generic, Eq)

recorded :: Lens' CampaignLog (HashSet CampaignLogKey)
recorded = lens campaignLogRecorded $ \m x -> m { campaignLogRecorded = x }

crossedOut :: Lens' CampaignLog (HashSet CampaignLogKey)
crossedOut =
  lens campaignLogCrossedOut $ \m x -> m { campaignLogCrossedOut = x }

recordedSets :: Lens' CampaignLog (HashMap CampaignLogKey [Recorded CardCode])
recordedSets =
  lens campaignLogRecordedSets $ \m x -> m { campaignLogRecordedSets = x }

recordedCounts :: Lens' CampaignLog (HashMap CampaignLogKey Int)
recordedCounts =
  lens campaignLogRecordedCounts $ \m x -> m { campaignLogRecordedCounts = x }

instance ToJSON CampaignLog where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignLog"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignLog"

instance FromJSON CampaignLog where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignLog"

mkCampaignLog :: CampaignLog
mkCampaignLog = CampaignLog
  { campaignLogRecorded = mempty
  , campaignLogCrossedOut = mempty
  , campaignLogRecordedCounts = mempty
  , campaignLogRecordedSets = mempty
  }
