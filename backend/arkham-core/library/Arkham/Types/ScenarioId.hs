module Arkham.Types.ScenarioId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype ScenarioId = ScenarioId { unScenarioId :: CardCode }
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

newtype CompletedScenarioId = CompletedScenarioId { unCompletedScenarioId :: ScenarioId }
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString)
