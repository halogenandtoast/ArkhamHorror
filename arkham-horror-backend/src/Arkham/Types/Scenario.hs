module Arkham.Types.Scenario where

import Arkham.Types.Location
import Arkham.Types.Stack
import Data.Text
import GHC.Generics
import Json
import Prelude (Show)

data ArkhamScenarioData = ArkhamScenarioData
  { arkhamScenarioId :: Text
  , arkhamScenarioName :: Text
  , arkhamScenarioFlavor :: Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkham") ArkhamScenarioData

data ArkhamScenario = ArkhamScenario
  { scenarioName :: Text
  , scenarioStacks :: [ArkhamStack] -- Should we call these decks
  , scenarioLocations :: [ArkhamLocation]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "scenario") ArkhamScenario
