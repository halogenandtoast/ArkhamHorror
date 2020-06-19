module Arkham.Types.Location where

import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data LocationContent = LocationClues Int | LocationInvestigator ArkhamInvestigator
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype LocationId = LocationId { unLocationId :: Text }
  deriving newtype (ToJSON, ToJSONKey, FromJSON, IsString, Eq, Hashable)

data ArkhamLocationSymbol = Circle | Heart
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamUnrevealedLocation = ArkhamUnrevealedLocation
  { aulName :: Text
  , aulLocationId :: LocationId
  , aulLocationSymbols :: [ArkhamLocationSymbol]
  , aulImage :: Text
  }
  deriving stock (Generic)

instance ToJSON ArkhamUnrevealedLocation where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamUnrevealedLocation where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

data ArkhamRevealedLocation = ArkhamRevealedLocation
  { arlName :: Text
  , arlLocationId :: LocationId
  , arlLocationSymbols :: [ArkhamLocationSymbol]
  , arlShroud :: Int
  , arlImage :: Text
  }
  deriving stock (Generic)

instance ToJSON ArkhamRevealedLocation where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamRevealedLocation where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

data ArkhamLocation = UnrevealedLocation ArkhamUnrevealedLocation | RevealedLocation ArkhamRevealedLocation
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

