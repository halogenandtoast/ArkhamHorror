module Arkham.Types.Scenario where

import Data.Text
import Database.Persist.Sql
import GHC.Generics
import Json
import Prelude (Show)

newtype ArkhamEncounterSet = ArkhamEncounterSet Text
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via TaggedJson "encounterSet" ArkhamEncounterSet

data ArkhamScenarioJsonData = ArkhamScenarioJsonData
  { arkhamScenarioDataFlavor :: Text
  , arkhamScenarioDataEncounterSets :: [ArkhamEncounterSet]
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkhamScenarioJsonData") ArkhamScenarioJsonData
  deriving (PersistField, PersistFieldSql) via PersistJson ArkhamScenarioJsonData
