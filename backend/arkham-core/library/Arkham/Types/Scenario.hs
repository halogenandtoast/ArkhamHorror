{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario
  ( lookupScenario
  , Scenario
  )
where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheGathering
import Arkham.Types.Scenario.Scenarios.TheMidnightMasks
import Arkham.Types.ScenarioId
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario =
  fromJustNote "Unknown scenario" . flip HashMap.lookup allScenarios

allScenarios :: HashMap ScenarioId (Difficulty -> Scenario)
allScenarios = HashMap.fromList
  [ ("01104", TheGathering' . theGathering)
  , ("01120", TheMidnightMasks' . theMidnightMasks)
  ]

data Scenario
  = TheGathering' TheGathering
  | TheMidnightMasks' TheMidnightMasks
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (ScenarioRunner env) => RunMessage env Scenario where
  runMessage msg = \case
    TheGathering' x -> TheGathering' <$> runMessage msg x
    TheMidnightMasks' x -> TheMidnightMasks' <$> runMessage msg x
