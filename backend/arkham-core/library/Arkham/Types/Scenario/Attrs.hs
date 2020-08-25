{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Attrs where

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
import ClassyPrelude
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
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "scenario"
  toEncoding = genericToEncoding $ aesonOptions $ Just "scenario"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenario"

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
    _ -> pure a
