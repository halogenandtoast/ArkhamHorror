module Arkham.Types.ScenarioId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype ScenarioId = ScenarioId { unScenarioId :: CardCode }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString)

newtype CompletedScenarioId = CompletedScenarioId { unCompletedScenarioId :: ScenarioId }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString)
