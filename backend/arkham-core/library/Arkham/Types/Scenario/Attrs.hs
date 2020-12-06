{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Attrs where

import Arkham.Import hiding (log)

import Arkham.Types.ScenarioLogKey
import Arkham.Types.Difficulty
import Arkham.Types.Scenario.Runner

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON)

instance HasAttrs Attrs where
  type AttrsT Attrs = Attrs
  toAttrs = id

data Attrs = Attrs
  { scenarioName        :: Text
  , scenarioId          :: ScenarioId
  , scenarioDifficulty  :: Difficulty
  -- These types are to handle complex scenarios with multiple stacks
  , scenarioAgendaStack :: [(Int, [AgendaId])] -- These types are to handle complex scenarios with multiple stacks
  , scenarioActStack    :: [(Int, [ActId])]
  , scenarioLocationLayout :: Maybe [GridTemplateRow]
  , scenarioDeck :: Maybe [EncounterCard]
  , scenarioLog :: HashSet ScenarioLogKey
  , scenarioLocations :: HashMap LocationName [LocationId]
  , scenarioSetAsideCards :: [Card]
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "scenario"
  toEncoding = genericToEncoding $ aesonOptions $ Just "scenario"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenario"

isEasyStandard :: Attrs -> Bool
isEasyStandard Attrs { scenarioDifficulty } =
  scenarioDifficulty `elem` [Easy, Standard]

isHardExpert :: Attrs -> Bool
isHardExpert Attrs { scenarioDifficulty } =
  scenarioDifficulty `elem` [Hard, Expert]

actStack :: Lens' Attrs [(Int, [ActId])]
actStack = lens scenarioActStack $ \m x -> m { scenarioActStack = x }

locations :: Lens' Attrs (HashMap LocationName [LocationId])
locations = lens scenarioLocations $ \m x -> m { scenarioLocations = x }

setAsideCards :: Lens' Attrs [Card]
setAsideCards =
  lens scenarioSetAsideCards $ \m x -> m { scenarioSetAsideCards = x }

log :: Lens' Attrs (HashSet ScenarioLogKey)
log = lens scenarioLog $ \m x -> m { scenarioLog = x }

baseAttrs :: CardCode -> Text -> [AgendaId] -> [ActId] -> Difficulty -> Attrs
baseAttrs cardCode name agendaStack actStack' difficulty = Attrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioAgendaStack = [(1, agendaStack)]
  , scenarioActStack = [(1, actStack')]
  , scenarioLocationLayout = Nothing
  , scenarioDeck = Nothing
  , scenarioLog = mempty
  , scenarioLocations = mempty
  , scenarioSetAsideCards = mempty
  }

instance Entity Attrs where
  type EntityId Attrs = ScenarioId
  toId = scenarioId
  toSource = ScenarioSource . toId
  toTarget = ScenarioTarget . toId
  isSource Attrs { scenarioId } (ScenarioSource sid) = scenarioId == sid
  isSource _ _ = False
  isTarget Attrs { scenarioId } (ScenarioTarget sid) = scenarioId == sid
  isTarget _ _ = False

instance HasTokenValue env InvestigatorId => HasTokenValue env Attrs where
  getTokenValue _ iid = \case
    ElderSign -> getTokenValue iid iid ElderSign
    AutoFail -> pure $ TokenValue AutoFail AutoFailModifier
    PlusOne -> pure $ TokenValue PlusOne (PositiveModifier 1)
    Zero -> pure $ TokenValue Zero (PositiveModifier 0)
    MinusOne -> pure $ TokenValue MinusOne (NegativeModifier 1)
    MinusTwo -> pure $ TokenValue MinusTwo (NegativeModifier 2)
    MinusThree -> pure $ TokenValue MinusThree (NegativeModifier 3)
    MinusFour -> pure $ TokenValue MinusFour (NegativeModifier 4)
    MinusFive -> pure $ TokenValue MinusFive (NegativeModifier 5)
    MinusSix -> pure $ TokenValue MinusSix (NegativeModifier 6)
    MinusSeven -> pure $ TokenValue MinusSeven (NegativeModifier 7)
    MinusEight -> pure $ TokenValue MinusEight (NegativeModifier 8)
    otherFace -> pure $ TokenValue otherFace NoModifier

instance ScenarioRunner env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Setup -> a <$ pushMessage BeginInvestigation
    PlaceLocationNamed locationName ->
      a <$ case findWithDefault [] locationName scenarioLocations of
        [] -> error "There were no locations with that name"
        [lid] -> unshiftMessage (PlaceLocation lid)
        _ ->
          error "We want there to be only one location when targetting names"
    EnemySpawnAtLocationNamed miid locationName eid ->
      a <$ case findWithDefault [] locationName scenarioLocations of
        [] -> error "There were no locations with that name"
        [lid] -> unshiftMessage (EnemySpawn miid lid eid)
        _ ->
          error "We want there to be only one location when targetting names"
    PlaceDoomOnAgenda -> do
      agendaIds <- getSetList @AgendaId ()
      case agendaIds of
        [] -> pure a
        [x] -> a <$ unshiftMessage (PlaceDoom (AgendaTarget x) 1)
        _ -> error "multiple agendas should be handled by the scenario"
    Discard (ActTarget _) -> pure $ a & actStack .~ []
    -- ^ See: Vengeance Awaits / The Devourer Below - right now the assumption
    -- | is that the act deck has been replaced.
    InvestigatorDefeated _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      if null investigatorIds then a <$ unshiftMessage NoResolution else pure a
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      if null investigatorIds then a <$ unshiftMessage NoResolution else pure a
    InvestigatorWhenEliminated iid ->
      a <$ unshiftMessage (InvestigatorEliminated iid)
    Remember logKey -> pure $ a & log %~ insertSet logKey
    ResolveToken _drawnToken token _iid | token == AutoFail ->
      a <$ unshiftMessage FailSkillTest
    NoResolution ->
      error "The scenario should specify what to do for no resolution"
    Resolution _ ->
      error "The scenario should specify what to do for the resolution"
    UseScenarioSpecificAbility{} ->
      error
        "The scenario should specify what to do for a scenario specific ability."
    _ -> pure a
