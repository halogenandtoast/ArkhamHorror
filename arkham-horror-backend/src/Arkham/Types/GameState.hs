module Arkham.Types.GameState where

import Arkham.Types.Player

import Arkham.Types.ChaosToken
import Arkham.Types.Location
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)

data ArkhamPhase = Mythos | Investigation | Enemy | Upkeep
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data ArkhamGameState = ArkhamGameState
  { agsPlayer :: ArkhamPlayer
  , agsPhase :: ArkhamPhase
  , agsChaosBag :: NonEmpty ArkhamChaosToken
  , agsLocations :: [ArkhamLocation]
  , agsLocationContents :: HashMap LocationId [LocationContent]
  , agsStacks :: [ArkhamStack]
  }
  deriving stock (Generic)

newtype ArkhamAct = ArkhamAct { aactImage :: Text }
  deriving newtype (ToJSON)

newtype ArkhamAgenda = ArkhamAgenda { aagendaImage :: Text }
  deriving newtype (ToJSON)

data ArkhamStack = ActStack ArkhamAct | AgendaStack ArkhamAgenda
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance ToJSON ArkhamGameState where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

