module Arkham.Types.CampaignStep where

import Arkham.Prelude

import Arkham.Types.ScenarioId

data CampaignStep
  = PrologueStep
  | ScenarioStep ScenarioId
  | InterludeStep Int (Maybe InterludeKey)
  | UpgradeDeckStep CampaignStep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InterludeKey = DanielSurvived | DanielWasPossessed | DanielDidNotSurvive
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
