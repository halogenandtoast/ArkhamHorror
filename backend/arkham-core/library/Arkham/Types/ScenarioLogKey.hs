module Arkham.Types.ScenarioLogKey where

import Arkham.Json
import ClassyPrelude

data ScenarioLogKey
  = FoundAStrangeDoll
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)
