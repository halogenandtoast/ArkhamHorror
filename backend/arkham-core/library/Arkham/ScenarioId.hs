module Arkham.ScenarioId where

import Arkham.Prelude

import Arkham.Card.CardCode

newtype ScenarioId = ScenarioId { unScenarioId :: CardCode }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype CompletedScenarioId = CompletedScenarioId { unCompletedScenarioId :: ScenarioId }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString)
