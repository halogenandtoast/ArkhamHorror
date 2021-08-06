module Arkham.Types.CampaignId where

import Arkham.Prelude

newtype CampaignId = CampaignId { unCampaignId :: Text }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)
