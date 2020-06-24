module Arkham.Types.Card where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

newtype ArkhamCardCode = ArkhamCardCode { unArkhamCardCode :: Text }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON)

data ArkhamPlayerCard = ArkhamPlayerCard
  { apcName :: Text
  , apcCost :: Maybe Int
  , apcCode :: ArkhamCardCode
  , apcImage :: Text
  , apcUses :: Maybe Int
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamPlayerCard where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamPlayerCard where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

data ArkhamEncounterCard = ArkhamEncounterCard
  { aecName :: Text
  , aecCode :: ArkhamCardCode
  , aecImage :: Text
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamEncounterCard where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamEncounterCard where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

data ArkhamCard = PlayerCard ArkhamPlayerCard | EncounterCard ArkhamEncounterCard
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
