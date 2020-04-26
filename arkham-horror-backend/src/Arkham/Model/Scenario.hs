{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Arkham.Model.Scenario where

import Arkham.Model.ProductSet
import Arkham.Types.Scenario
import Data.Text
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Json
import Prelude (Show)

mkPersist sqlSettings [persistLowerCase|
ArkhamScenario sql=arkham_scenarios
  title Text
  productSetId ArkhamProductSetId
  data ArkhamScenarioJsonData
  ScenarioTitleUnique title
  deriving Generic Show
|]

deriving via (Codec (Drop "arkhamScenario") ArkhamScenario) instance ToJSON ArkhamScenario
deriving via (Codec (Drop "arkhamScenario") ArkhamScenario) instance FromJSON ArkhamScenario

