{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.CampaignLog where

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes.HasRecord
import ClassyPrelude
import Lens.Micro.TH

data CampaignLog = CampaignLog
  { campaignLogRecorded :: HashSet CampaignLogKey
  , campaignLogRecordedCounts :: HashMap CampaignLogKey Int
  , campaignLogRecordedSets :: HashMap CampaignLogKey [CardCode]
  }
  deriving stock (Show, Generic)

makeFields ''CampaignLog

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
