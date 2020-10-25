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
import Arkham.Types.ScenarioLogKey
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
deriving anyclass instance (ScenarioRunner env, HasTokenValue env InvestigatorId) => HasTokenValue env Scenario

instance HasSet ScenarioLogKey () Scenario where
  getSet _ = scenarioLog . scenarioAttrs

newtype BaseScenario = BaseScenario Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasTokenValue env InvestigatorId, HasQueue env) => HasTokenValue env BaseScenario where
  getTokenValue (BaseScenario attrs) iid token = case drawnTokenFace token of
    Token.Skull -> pure $ TokenValue token (NegativeModifier 1)
    Token.Cultist -> pure $ TokenValue token (NegativeModifier 1)
    Token.Tablet -> pure $ TokenValue token (NegativeModifier 1)
    Token.ElderThing -> pure $ TokenValue token (NegativeModifier 1)
    _other -> getTokenValue attrs iid token

instance (ScenarioRunner env) => RunMessage env BaseScenario where
  runMessage msg a@(BaseScenario attrs) = case msg of
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
