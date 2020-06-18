module Arkham.Types.Location where

import Arkham.Types.Investigator
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data LocationContent = LocationClues Int | LocationInvestigator ArkhamInvestigator
  deriving stock (Generic)
  deriving anyclass (ToJSON)

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

class HasLocationId a where
  getLocationId :: a -> LocationId

instance HasLocationId ArkhamRevealedLocation where
  getLocationId = arlLocationId

instance HasLocationId ArkhamUnrevealedLocation where
  getLocationId = aulLocationId

instance HasLocationId ArkhamLocation where
  getLocationId (UnrevealedLocation l) = getLocationId l
  getLocationId (RevealedLocation l) = getLocationId l

data ArkhamLocation = UnrevealedLocation ArkhamUnrevealedLocation | RevealedLocation ArkhamRevealedLocation
  deriving stock (Generic)
  deriving anyclass (ToJSON)

