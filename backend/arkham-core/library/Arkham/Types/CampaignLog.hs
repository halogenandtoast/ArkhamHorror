module Arkham.Types.CampaignLog where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes.HasRecord

data CampaignLog = CampaignLog
  { campaignLogRecorded :: Set CampaignLogKey
  , campaignLogRecordedCounts :: Map CampaignLogKey Int
  , campaignLogRecordedSets :: Map CampaignLogKey [CardCode]
  }
  deriving stock (Show, Generic, Eq)

recorded :: Lens' CampaignLog (Set CampaignLogKey)
recorded = lens campaignLogRecorded $ \m x -> m { campaignLogRecorded = x }

recordedSets :: Lens' CampaignLog (Map CampaignLogKey [CardCode])
recordedSets =
  lens campaignLogRecordedSets $ \m x -> m { campaignLogRecordedSets = x }

recordedCounts :: Lens' CampaignLog (Map CampaignLogKey Int)
recordedCounts =
  lens campaignLogRecordedCounts $ \m x -> m { campaignLogRecordedCounts = x }

instance ToJSON CampaignLog where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignLog"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignLog"

instance FromJSON CampaignLog where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignLog"

instance HasRecord CampaignLog where
  hasRecord key = asks $ member key . campaignLogRecorded
  hasRecordSet key = asks $ findWithDefault [] key . campaignLogRecordedSets
  hasRecordCount key = asks $ findWithDefault 0 key . campaignLogRecordedCounts

mkCampaignLog :: CampaignLog
mkCampaignLog = CampaignLog
  { campaignLogRecorded = mempty
  , campaignLogRecordedCounts = mempty
  , campaignLogRecordedSets = mempty
  }
