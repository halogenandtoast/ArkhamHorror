module Arkham.CampaignStep where

import Arkham.Prelude

import Arkham.Id

data CampaignStep
  = PrologueStep
  | ScenarioStep ScenarioId
  | ScenarioStepPart ScenarioId Int
  | InterludeStep Int (Maybe InterludeKey)
  | InterludeStepPart Int (Maybe InterludeKey) Int
  | UpgradeDeckStep CampaignStep
  | EpilogueStep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InterludeKey = DanielSurvived | DanielWasPossessed | DanielDidNotSurvive
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
