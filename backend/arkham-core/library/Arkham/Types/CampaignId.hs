module Arkham.Types.CampaignId where

import ClassyPrelude
import Data.Aeson

newtype CampaignId = CampaignId { unCampaignId :: Text }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)
