module Arkham.Types.Scenario where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamScenario = ArkhamScenario
  { asName :: Text
  , asGuide :: Text
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamScenario where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }

instance FromJSON ArkhamScenario where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
