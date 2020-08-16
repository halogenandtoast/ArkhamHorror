module Arkham.Types.CampaignStep where

import Arkham.Json
import Arkham.Types.ScenarioId
import ClassyPrelude

data CampaignStep = PrologueStep | ScenarioStep ScenarioId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
