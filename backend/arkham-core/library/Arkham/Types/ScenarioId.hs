module Arkham.Types.ScenarioId where

import ClassyPrelude

import Arkham.Types.Card.CardCode
import Data.Aeson

newtype ScenarioId = ScenarioId { unScenarioId :: CardCode }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString)

newtype CompletedScenarioId = CompletedScenarioId { unCompletedScenarioId :: ScenarioId }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString)
