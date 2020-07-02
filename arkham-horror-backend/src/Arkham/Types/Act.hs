module Arkham.Types.Act
  ( ArkhamAct(..)
  )
where

import Arkham.Types.Card
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamAct = ArkhamAct { aactCardCode :: ArkhamCardCode, aactImage :: Text }
  deriving stock (Show, Generic)

instance ToJSON ArkhamAct where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

instance FromJSON ArkhamAct where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

