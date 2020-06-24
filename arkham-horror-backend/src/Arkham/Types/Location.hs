module Arkham.Types.Location where

import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data LocationContent = LocationClues Int | LocationInvestigator ArkhamInvestigator
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype LocationId = LocationId { unLocationId :: Text }
  deriving newtype (Show, ToJSON, ToJSONKey, FromJSONKey, FromJSON, IsString, Eq, Hashable)

data ArkhamLocationSymbol = Circle | Heart
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamUnrevealedLocation = ArkhamUnrevealedLocation
  { aulName :: Text
  , aulLocationId :: LocationId
  , aulLocationSymbols :: [ArkhamLocationSymbol]
  , aulImage :: Text
  , aulContents :: [LocationContent]
  }
  deriving stock (Generic, Show)

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
  , arlContents :: [LocationContent]
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamRevealedLocation where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamRevealedLocation where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

data ArkhamLocation = UnrevealedLocation ArkhamUnrevealedLocation | RevealedLocation ArkhamRevealedLocation
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

