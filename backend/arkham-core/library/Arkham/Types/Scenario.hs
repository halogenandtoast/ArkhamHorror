{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario
  ( lookupScenario
  , baseScenario
  , difficultyOfScenario
  , Scenario
  )
where

import Arkham.Import

import Arkham.Types.Difficulty
import Arkham.Types.Helpers
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.CurseOfTheRougarou
import Arkham.Types.Scenario.Scenarios.TheDevourerBelow
import Arkham.Types.Scenario.Scenarios.TheGathering
import Arkham.Types.Scenario.Scenarios.TheMidnightMasks
import Arkham.Types.ScenarioId
import qualified Arkham.Types.Token as Token
import Data.Coerce

data Scenario
  = TheGathering' TheGathering
  | TheMidnightMasks' TheMidnightMasks
  | TheDevourerBelow' TheDevourerBelow
  | CurseOfTheRougarou' CurseOfTheRougarou
  | BaseScenario' BaseScenario
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance (ScenarioRunner env) => RunMessage env Scenario

newtype BaseScenario = BaseScenario Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (ScenarioRunner env) => RunMessage env BaseScenario where
  runMessage msg a@(BaseScenario attrs) = case msg of
    ResolveToken Token.Skull iid ->
      a <$ runTest iid (Token.TokenValue Token.Skull (-1))
    ResolveToken Token.Cultist iid ->
      a <$ runTest iid (Token.TokenValue Token.Cultist (-1))
    ResolveToken Token.Tablet iid ->
      a <$ runTest iid (Token.TokenValue Token.Tablet (-1))
    ResolveToken Token.ElderThing iid ->
      a <$ runTest iid (Token.TokenValue Token.ElderThing (-1))
    NoResolution -> pure a
    _ -> BaseScenario <$> runMessage msg attrs

baseScenario
  :: CardCode
  -> Text
  -> [AgendaId]
  -> [ActId]
  -> Difficulty
  -> (Attrs -> Attrs)
  -> Scenario
baseScenario a b c d e f =
  BaseScenario' . BaseScenario . f $ baseAttrs a b c d e

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario = fromJustNote "Unknown scenario" . flip lookup allScenarios

difficultyOfScenario :: Scenario -> Difficulty
difficultyOfScenario = scenarioDifficulty . scenarioAttrs

allScenarios :: HashMap ScenarioId (Difficulty -> Scenario)
allScenarios = mapFromList
  [ ("01104", TheGathering' . theGathering)
  , ("01120", TheMidnightMasks' . theMidnightMasks)
  , ("01142", TheDevourerBelow' . theDevourerBelow)
  , ("81001", CurseOfTheRougarou' . curseOfTheRougarou)
  ]

scenarioAttrs :: Scenario -> Attrs
scenarioAttrs = \case
  BaseScenario' attrs -> coerce attrs
  TheGathering' attrs -> coerce attrs
  TheMidnightMasks' attrs -> coerce attrs
  TheDevourerBelow' attrs -> coerce attrs
  CurseOfTheRougarou' (CurseOfTheRougarou (attrs `With` _)) -> attrs
