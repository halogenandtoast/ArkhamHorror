{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario
  ( lookupScenario
  , Scenario
  )
where

import Arkham.Json
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Arkham.Types.Source
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)
import System.Random.Shuffle

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario =
  fromJustNote "Unknown scenario" . flip HashMap.lookup allScenarios

allScenarios :: HashMap ScenarioId (Difficulty -> Scenario)
allScenarios =
  HashMap.fromList [("01104", theGathering), ("01120", theMidnightMasks)]

data Attrs = Attrs
  { scenarioName        :: Text
  , scenarioId          :: ScenarioId
  , scenarioDifficulty  :: Difficulty
  -- These types are to handle complex scenarios with multiple stacks
  , scenarioAgendaStack :: [(Int, [AgendaId])] -- These types are to handle complex scenarios with multiple stacks
  , scenarioActStack    :: [(Int, [ActId])]
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "scenario"
  toEncoding = genericToEncoding $ aesonOptions $ Just "scenario"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenario"

data Scenario
  = TheGathering TheGatheringI
  | TheMidnightMasks TheMidnightMasksI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- scenarioAttrs :: Scenario -> Attrs
-- scenarioAttrs = \case
--   TheGathering attrs -> coerce attrs
--   TheMidnightMasks attrs -> coerce attrs

baseAttrs :: CardCode -> Text -> [AgendaId] -> [ActId] -> Difficulty -> Attrs
baseAttrs cardCode name agendaStack actStack difficulty = Attrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioAgendaStack = [(1, agendaStack)]
  , scenarioActStack = [(1, actStack)]
  }

newtype TheGatheringI = TheGatheringI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theGathering :: Difficulty -> Scenario
theGathering = TheGathering . TheGatheringI . baseAttrs
  "01104"
  "The Gathering"
  ["01105", "01106", "01107"]
  ["01108", "01109", "01110"]

newtype TheMidnightMasksI = TheMidnightMasksI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMidnightMasks :: Difficulty -> Scenario
theMidnightMasks =
  TheMidnightMasks
    . TheMidnightMasksI
    . baseAttrs "01120" "The Midnight Masks" [] []

type ScenarioRunner env
  = (HasCount EnemyCount (InvestigatorLocation, [Trait]) env, HasQueue env)

instance (ScenarioRunner env) => RunMessage env Scenario where
  runMessage msg = \case
    TheGathering x -> TheGathering <$> runMessage msg x
    TheMidnightMasks x -> TheMidnightMasks <$> runMessage msg x

instance (ScenarioRunner env) => RunMessage env TheGatheringI where
  runMessage msg s@(TheGatheringI attrs@Attrs {..}) = case msg of
    Setup -> do
      encounterDeck <- liftIO $ shuffleM . concat =<< traverse
        gatherEncounterSet
        [ EncounterSet.TheGathering
        , EncounterSet.Rats
        , EncounterSet.Ghouls
        , EncounterSet.StrikingFear
        , EncounterSet.ChillingCold
        ]
      pushMessages
        [ SetEncounterDeck encounterDeck
        , AddAgenda "01105"
        , AddAct "01108"
        , PlaceLocation "01111"
        , RevealLocation "01111"
        , MoveAllTo "01111"
        ]
      TheGatheringI <$> runMessage msg attrs
    ResolveToken Token.Skull iid skillValue ->
      if scenarioDifficulty `elem` [Easy, Standard]
        then do
          ghoulCount <- unEnemyCount
            <$> asks (getCount (InvestigatorLocation iid, [Ghoul]))
          s <$ runTest (skillValue - ghoulCount)
        else do
          unshiftMessage
            (AddOnFailure $ FindAndDrawEncounterCard iid (EnemyType, Ghoul))
          s <$ runTest (skillValue - 2)
    ResolveToken Token.Cultist iid skillValue ->
      if scenarioDifficulty `elem` [Easy, Standard]
        then do
          unshiftMessage
            (AddOnFailure
            $ InvestigatorAssignDamage iid (TokenSource Token.Cultist) 0 1
            )
          s <$ runTest (skillValue - 1)
        else do
          unshiftMessage (DrawAnotherToken iid skillValue Token.Cultist)
          unshiftMessage
            (AddOnFailure
            $ InvestigatorAssignDamage iid (TokenSource Token.Cultist) 0 2
            )
          pure s
    ResolveToken Token.Tablet iid skillValue -> do
      ghoulCount <- unEnemyCount
        <$> asks (getCount (InvestigatorLocation iid, [Ghoul]))
      if scenarioDifficulty `elem` [Easy, Standard]
        then do
          when (ghoulCount > 0) $ unshiftMessage
            (InvestigatorAssignDamage iid (TokenSource Token.Tablet) 1 0)
          s <$ runTest (skillValue - 2)
        else do
          when (ghoulCount > 0) $ unshiftMessage
            (InvestigatorAssignDamage iid (TokenSource Token.Tablet) 1 1)
          s <$ runTest (skillValue - 4)
    _ -> pure s

instance (ScenarioRunner env) => RunMessage env TheMidnightMasksI where
  runMessage msg (TheMidnightMasksI attrs) =
    TheMidnightMasksI <$> runMessage msg attrs

instance (ScenarioRunner env) => RunMessage env Attrs where
  runMessage msg a = case msg of
    Setup -> a <$ pushMessage BeginInvestigation
    _ -> pure a
