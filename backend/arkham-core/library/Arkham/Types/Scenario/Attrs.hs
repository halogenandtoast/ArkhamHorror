{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Attrs where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Scenario.Runner
import Arkham.Types.ScenarioId
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token
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

baseAttrs :: CardCode -> Text -> [AgendaId] -> [ActId] -> Difficulty -> Attrs
baseAttrs cardCode name agendaStack actStack' difficulty = Attrs
  { scenarioId = ScenarioId cardCode
  , scenarioName = name
  , scenarioDifficulty = difficulty
  , scenarioAgendaStack = [(1, agendaStack)]
  , scenarioActStack = [(1, actStack')]
  , scenarioLocationLayout = Nothing
  , scenarioDeck = Nothing
  }

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
    NoResolution ->
      error "The scenario should specify what to do for no resolution"
    Resolution _ ->
      error "The scenario should specify what to do for the resolution"
    UseScenarioSpecificAbility{} ->
      error
        "The scenario should specify what to do for a scenario specific ability."
    ResolveToken Token.PlusOne iid ->
      a <$ runTest iid (Token.TokenValue Token.PlusOne 1)
    ResolveToken Token.Zero iid ->
      a <$ runTest iid (Token.TokenValue Token.Zero 0)
    ResolveToken Token.MinusOne iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusOne (-1))
    ResolveToken Token.MinusTwo iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusTwo (-2))
    ResolveToken Token.MinusThree iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusThree (-3))
    ResolveToken Token.MinusFour iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusFour (-4))
    ResolveToken Token.MinusFive iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusFive (-5))
    ResolveToken Token.MinusSix iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusSix (-6))
    ResolveToken Token.MinusSeven iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusSeven (-7))
    ResolveToken Token.MinusEight iid ->
      a <$ runTest iid (Token.TokenValue Token.MinusEight (-8))
    ResolveToken Token.AutoFail _ -> a <$ unshiftMessage FailSkillTest
    _ -> pure a
