module Arkham.CampaignStep where

import Arkham.Id
import Arkham.Prelude

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
  | CheckpointStep Int
  deriving stock (Show, Eq, Generic, Data)
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
  | -- The Innsmouth Conspiracy
    HasPurpleKey
  | HasWhiteKey
  | HasBlackKey
  | HasPurpleAndWhiteKeys
  | HasPurpleAndBlackKeys
  | HasWhiteAndBlackKeys
  | HasPurpleWhiteAndBlackKeys
  | HasNoKeys
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
