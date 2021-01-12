module Arkham.Types.CampaignStep where

import Arkham.Prelude

import Arkham.Types.ScenarioId

data CampaignStep = PrologueStep | ScenarioStep ScenarioId | InterludeStep Int | UpgradeDeckStep CampaignStep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
