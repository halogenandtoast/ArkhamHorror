module Arkham.Types.GameState where

import Arkham.Types.Player

import Arkham.Types.Action
import Arkham.Types.ChaosToken
import Arkham.Types.Location
import Arkham.Types.Skill
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)

data ArkhamPhase = Mythos | Investigation | Enemy | Upkeep
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamGameStateStep
  = ArkhamGameStateStepInvestigatorActionStep
  | ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype ArkhamTarget = LocationTarget ArkhamLocation
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamSkillCheckStep = ArkhamSkillCheckStep
  { ascsType :: ArkhamSkillType
  , ascsAction :: Maybe ArkhamAction
  , ascsTarget :: Maybe ArkhamTarget
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamSkillCheckStep where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

instance FromJSON ArkhamSkillCheckStep where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

data ArkhamGameState = ArkhamGameState
  { agsPlayer :: ArkhamPlayer
  , agsPhase :: ArkhamPhase
  , agsChaosBag :: NonEmpty ArkhamChaosToken
  , agsLocations :: [ArkhamLocation]
  , agsLocationContents :: HashMap LocationId [LocationContent]
  , agsStacks :: [ArkhamStack]
  , agsStep :: ArkhamGameStateStep
  }
  deriving stock (Generic, Show)

newtype ArkhamAct = ArkhamAct { aactImage :: Text }
  deriving newtype (Show, ToJSON, FromJSON)

newtype ArkhamAgenda = ArkhamAgenda { aagendaImage :: Text }
  deriving newtype (Show, ToJSON, FromJSON)

data ArkhamStack = ActStack ArkhamAct | AgendaStack ArkhamAgenda
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToJSON ArkhamGameState where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamGameState where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
