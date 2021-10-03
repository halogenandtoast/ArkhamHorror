module Arkham.Types.CampaignLog where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes.HasRecord

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

instance HasRecord env CampaignLog where
  hasRecord key = pure . member key . campaignLogRecorded
  hasRecordSet key = pure . findWithDefault [] key . campaignLogRecordedSets
  hasRecordCount key = pure . findWithDefault 0 key . campaignLogRecordedCounts

mkCampaignLog :: CampaignLog
mkCampaignLog = CampaignLog
  { campaignLogRecorded = mempty
  , campaignLogCrossedOut = mempty
  , campaignLogRecordedCounts = mempty
  , campaignLogRecordedSets = mempty
  }
