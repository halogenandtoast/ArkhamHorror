module Arkham.Types.GameState
  ( ArkhamGameState(..)
  , ArkhamGameStateLock(..)
  , ArkhamGameStateStep(..)
  , ArkhamStack(..)
  , ArkhamPhase(..)
  , ArkhamAct(..)
  , ArkhamAgenda(..)
  , ArkhamSkillCheckStep(..)
  , ArkhamRevealTokenStep(..)
  , ArkhamTarget(..)
  )
where

import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Enemy
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Skill
import Base.Lock
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)
import Data.UUID
import Lens.Micro

data ArkhamPhase = Mythos | Investigation | Enemy | Upkeep
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamGameStateStep
  = ArkhamGameStateStepInvestigatorActionStep
  | ArkhamGameStateStepSkillCheckStep ArkhamSkillCheckStep
  | ArkhamGameStateStepRevealTokenStep ArkhamRevealTokenStep
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Eq ArkhamGameStateStep where
  ArkhamGameStateStepInvestigatorActionStep == ArkhamGameStateStepInvestigatorActionStep
    = True
  ArkhamGameStateStepSkillCheckStep _ == ArkhamGameStateStepSkillCheckStep _
    = True
  ArkhamGameStateStepRevealTokenStep _ == ArkhamGameStateStepRevealTokenStep _
    = True
  _ == _ = False


data ArkhamTarget = LocationTarget ArkhamLocation | OtherTarget ArkhamLocation
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

data ArkhamRevealTokenStep = ArkhamRevealTokenStep
  { artsType :: ArkhamSkillType
  , artsAction :: Maybe ArkhamAction
  , artsTarget :: Maybe ArkhamTarget
  , artsToken :: ArkhamChaosToken
  , artsDifficulty :: Int
  , artsModifiedSkillValue :: Int
  , artsCards :: [ArkhamCard]
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamRevealTokenStep where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

instance FromJSON ArkhamRevealTokenStep where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

data ArkhamGameStateLock = AddDoom | InvestigationTakeActions | UpkeepResetActions | DrawAndGainResources
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ArkhamGameState = ArkhamGameState
  { agsPlayer :: ArkhamPlayer
  , agsPhase :: ArkhamPhase
  , agsChaosBag :: NonEmpty ArkhamChaosToken
  , agsLocations :: HashMap ArkhamCardCode ArkhamLocation
  , agsEnemies :: HashMap UUID ArkhamEnemy
  , agsStacks :: HashMap Text ArkhamStack
  , agsEncounterDeck :: [ArkhamEncounterCard]
  , agsStep :: ArkhamGameStateStep
  , agsLock :: Maybe ArkhamGameStateLock
  }
  deriving stock (Generic, Show)

instance HasLock ArkhamGameState where
  type LockKey ArkhamGameState = ArkhamGameStateLock
  lock = lens agsLock $ \m x -> m { agsLock = x }

data ArkhamAct = ArkhamAct { aactCardCode :: ArkhamCardCode, aactImage :: Text }
  deriving stock (Show, Generic)

instance ToJSON ArkhamAct where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

instance FromJSON ArkhamAct where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

data ArkhamAgenda = ArkhamAgenda { aagendaCardCode :: ArkhamCardCode, aagendaImage :: Text, aagendaDoom :: Int }
  deriving stock (Show, Generic)

instance ToJSON ArkhamAgenda where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 7 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 7 }

instance FromJSON ArkhamAgenda where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 7 }

data ArkhamStack = ActStack (NonEmpty ArkhamAct) | AgendaStack (NonEmpty ArkhamAgenda)
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
