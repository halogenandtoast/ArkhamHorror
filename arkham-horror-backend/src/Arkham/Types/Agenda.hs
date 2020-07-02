module Arkham.Types.Agenda
  ( ArkhamAgenda(..)
  )
where

import Arkham.Types.Card
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamAgenda = ArkhamAgenda { aagendaCardCode :: ArkhamCardCode, aagendaImage :: Text, aagendaDoom :: Int }
  deriving stock (Show, Generic)

instance ToJSON ArkhamAgenda where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 7 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 7 }

instance FromJSON ArkhamAgenda where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 7 }
