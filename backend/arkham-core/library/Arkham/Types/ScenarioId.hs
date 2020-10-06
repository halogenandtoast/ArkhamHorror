module Arkham.Types.ScenarioId where

import Arkham.Types.Card.CardCode
import ClassyPrelude
import Data.Aeson

newtype ScenarioId = ScenarioId CardCode
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString)
