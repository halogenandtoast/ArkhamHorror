module Arkham.Types.CampaignLog where

import Arkham.Json
import ClassyPrelude

data CampaignLogKey
  = GhoulPriestAlive
  | HouseStanding
  | CultistsWhoGotAway
  | PastMidnight
  | LitaForcedToFindOther
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)

data CampaignLog = CampaignLog
  { campaignLogRecorded :: HashSet CampaignLogKey
  , campaignLogNumeric :: HashMap CampaignLogKey Int
  }
  deriving stock (Show, Generic)

mkCampaignLog :: CampaignLog
mkCampaignLog =
  CampaignLog { campaignLogRecorded = mempty, campaignLogNumeric = mempty }

instance ToJSON CampaignLog where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignLog"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignLog"

instance FromJSON CampaignLog where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignLog"
