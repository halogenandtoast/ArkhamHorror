{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Attrs where

import ClassyPrelude hiding (log)

import Arkham.Json
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Difficulty
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Scenario.Runner
import Arkham.Types.ScenarioId
import Arkham.Types.Target
import Arkham.Types.Token
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype GridTemplateRow = GridTemplateRow { unGridTemplateRow :: Text }
  deriving newtype (Show, IsString, ToJSON, FromJSON)

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
  }

instance (HasTokenValue env InvestigatorId, HasQueue env) => HasTokenValue env Attrs where
  getTokenValue _ iid token = do
    case drawnTokenFace token of
      ElderSign -> getTokenValue iid iid token
      AutoFail -> pure $ TokenValue token AutoFailModifier
      PlusOne -> pure $ TokenValue token (PositiveModifier 1)
      Zero -> pure $ TokenValue token (PositiveModifier 0)
      MinusOne -> pure $ TokenValue token (NegativeModifier 1)
      MinusTwo -> pure $ TokenValue token (NegativeModifier 2)
      MinusThree -> pure $ TokenValue token (NegativeModifier 3)
      MinusFour -> pure $ TokenValue token (NegativeModifier 4)
      MinusFive -> pure $ TokenValue token (NegativeModifier 5)
      MinusSix -> pure $ TokenValue token (NegativeModifier 6)
      MinusSeven -> pure $ TokenValue token (NegativeModifier 7)
      MinusEight -> pure $ TokenValue token (NegativeModifier 8)
      _ -> pure $ TokenValue token NoModifier

instance (ScenarioRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Setup -> a <$ pushMessage BeginInvestigation
    PlaceDoomOnAgenda -> do
      agendaIds <- HashSet.toList <$> asks (getSet @AgendaId ())
      case agendaIds of
        [] -> pure a
        [x] -> a <$ unshiftMessage (PlaceDoom (AgendaTarget x) 1)
        _ -> error "multiple agendas should be handled by the scenario"
    Discard (ActTarget _) -> pure $ a & actStack .~ []
    -- ^ See: Vengeance Awaits / The Devourer Below - right now the assumption
    -- | is that the act deck has been replaced.
    InvestigatorDefeated _ -> do
      investigatorIds <- asks (getSet @InScenarioInvestigatorId ())
      if null investigatorIds then a <$ unshiftMessage NoResolution else pure a
    InvestigatorResigned _ -> do
      investigatorIds <- asks (getSet @InScenarioInvestigatorId ())
      if null investigatorIds then a <$ unshiftMessage NoResolution else pure a
    InvestigatorWhenEliminated iid -> do
      a <$ unshiftMessage (InvestigatorEliminated iid)
    Remember logKey -> do
      pure $ a & log %~ insertSet logKey
    ResolveToken token _iid | drawnTokenFace token == AutoFail ->
      a <$ unshiftMessage FailSkillTest
    NoResolution ->
      error "The scenario should specify what to do for no resolution"
    Resolution _ ->
      error "The scenario should specify what to do for the resolution"
    UseScenarioSpecificAbility{} ->
      error
        "The scenario should specify what to do for a scenario specific ability."
    _ -> pure a
