module Arkham.Types.Scenario where

import Data.Text
import Database.Persist.Sql
import GHC.Generics
import Json
import Prelude (Show)

newtype ArkhamEncounterSetJson = ArkhamEncounterSetJson Text
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via TaggedJson "encounterSet" ArkhamEncounterSetJson

data ArkhamScenarioJsonData = ArkhamScenarioJsonData
  { arkhamScenarioDataFlavor :: Text
  , arkhamScenarioDataEncounterSets :: [ArkhamEncounterSetJson]
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via Codec (Drop "arkhamScenarioJsonData") ArkhamScenarioJsonData
  deriving (PersistField, PersistFieldSql) via PersistJson ArkhamScenarioJsonData
