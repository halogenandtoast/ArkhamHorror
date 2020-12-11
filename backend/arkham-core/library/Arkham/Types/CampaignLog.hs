module Arkham.Types.CampaignLog where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes.HasRecord
import Control.Lens

data CampaignLog = CampaignLog
  { campaignLogRecorded :: HashSet CampaignLogKey
  , campaignLogRecordedCounts :: HashMap CampaignLogKey Int
  , campaignLogRecordedSets :: HashMap CampaignLogKey [CardCode]
  }
  deriving stock (Show, Generic)

recorded :: Lens' CampaignLog (HashSet CampaignLogKey)
recorded = lens campaignLogRecorded $ \m x -> m { campaignLogRecorded = x }

recordedSets :: Lens' CampaignLog (HashMap CampaignLogKey [CardCode])
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

instance HasRecord CampaignLog where
  hasRecord key = member key . campaignLogRecorded
  hasRecordSet key = findWithDefault [] key . campaignLogRecordedSets

mkCampaignLog :: CampaignLog
mkCampaignLog = CampaignLog
  { campaignLogRecorded = mempty
  , campaignLogRecordedCounts = mempty
  , campaignLogRecordedSets = mempty
  }
