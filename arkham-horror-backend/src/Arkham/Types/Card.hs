module Arkham.Types.Card where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamPlayerCard = ArkhamPlayerCard
  { apcName :: Text
  , apcCost :: Maybe Int
  , apcImage :: Text
  }
  deriving stock (Generic)

instance ToJSON ArkhamPlayerCard where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

data ArkhamEncounterCard = ArkhamEncounterCard
  { aecName :: Text
  , aecImage :: Text
  }
  deriving stock (Generic)

instance ToJSON ArkhamEncounterCard where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

data ArkhamCard = PlayerCard ArkhamPlayerCard | EncounterCard ArkhamEncounterCard
  deriving stock (Generic)
  deriving anyclass (ToJSON)

