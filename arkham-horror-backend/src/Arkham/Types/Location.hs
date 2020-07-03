module Arkham.Types.Location
  ( ArkhamLocation(..)
  , ArkhamLocationStatus(..)
  , ArkhamLocationSymbol(..)
  )
where

import Arkham.Types.Card
import ClassyPrelude hiding (Index)
import Data.Aeson
import Data.Aeson.Casing
import Data.UUID

data ArkhamLocationSymbol = Circle | Heart
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamLocationStatus = Revealed | Unrevealed | OutOfPlay
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamLocation = ArkhamLocation
  { alName :: Text
  , alCardCode :: ArkhamCardCode
  , alLocationSymbol :: Maybe ArkhamLocationSymbol
  , alConnectedLocationSymbols :: [ArkhamLocationSymbol]
  , alShroud :: Int
  , alImage :: Text
  , alInvestigators :: [UUID]
  , alEnemies :: [UUID]
  , alClues :: Int
  , alDoom :: Int
  , alStatus :: ArkhamLocationStatus
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamLocation where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }

instance FromJSON ArkhamLocation where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
