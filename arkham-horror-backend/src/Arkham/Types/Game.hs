module Arkham.Types.Game where

import Arkham.Types.GameState
import Arkham.Types.Scenario
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamCycle = NightOfTheZealot | TheDunwichLegacy
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamGame = ArkhamGame
  { agId :: Int
  , agCycle :: ArkhamCycle
  , agScenario :: ArkhamScenario
  , agGameState :: ArkhamGameState
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamGame where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }

instance FromJSON ArkhamGame where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
