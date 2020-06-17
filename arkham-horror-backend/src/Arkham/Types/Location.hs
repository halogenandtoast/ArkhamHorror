module Arkham.Types.Location where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

newtype LocationId = LocationId { unLocationId :: Text }
  deriving newtype (ToJSON, ToJSONKey, IsString, Eq, Hashable)

data ArkhamLocationSymbol = Circle | Heart
  deriving stock (Generic)
  deriving anyclass (ToJSON)

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

data ArkhamLocation = UnrevealedLocation ArkhamUnrevealedLocation | RevealedLocation ArkhamRevealedLocation
  deriving stock (Generic)
  deriving anyclass (ToJSON)

