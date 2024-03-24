module Arkham.CampaignStep where

import Arkham.Prelude

import Arkham.Id

data CampaignStep
  = PrologueStep
  | PrologueStepPart Int
  | ScenarioStep ScenarioId
  | ScenarioStepPart ScenarioId Int
  | InterludeStep Int (Maybe InterludeKey)
  | InterludeStepPart Int (Maybe InterludeKey) Int
  | UpgradeDeckStep CampaignStep
  | EpilogueStep
  | EpilogueStepPart Int
  | InvestigatorCampaignStep InvestigatorId CampaignStep
  | ResupplyPoint
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InterludeKey
  = DanielSurvived
  | DanielWasPossessed
  | DanielDidNotSurvive
  | TheCustodianWasUnderControl
  | -- The Circle Undone
    ThePriceOfProgress4
  | ThePriceOfProgress5
  | ThePriceOfProgress6
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
