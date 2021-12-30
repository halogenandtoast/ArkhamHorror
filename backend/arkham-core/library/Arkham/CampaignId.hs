module Arkham.CampaignId where

import Arkham.Prelude

newtype CampaignId = CampaignId { unCampaignId :: Text }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)
