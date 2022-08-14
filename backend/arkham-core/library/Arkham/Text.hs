module Arkham.Text where

import Arkham.Prelude
import Arkham.Json

newtype Tooltip = Tooltip Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data FlavorText = FlavorText
  { flavorTitle :: Maybe Text
  , flavorBody :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FlavorText where
  toJSON = genericToJSON $ aesonOptions $ Just "flavor"

instance FromJSON FlavorText where
  parseJSON = genericParseJSON $ aesonOptions $ Just "flavor"
