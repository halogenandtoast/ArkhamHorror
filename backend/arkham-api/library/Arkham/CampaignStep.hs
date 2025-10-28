module Arkham.CampaignStep where

import Arkham.Id
import Arkham.Prelude
import GHC.Records

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
  | CampaignSpecificStep Text
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "normalize" CampaignStep CampaignStep where
  getField = normalizedCampaignStep

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
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

normalizedCampaignStep :: CampaignStep -> CampaignStep
normalizedCampaignStep = \case
  PrologueStep -> PrologueStep
  PrologueStepPart _ -> PrologueStep
  ScenarioStep sid -> ScenarioStep sid
  ScenarioStepPart sid _ -> ScenarioStep sid
  InterludeStep n _ -> InterludeStep n Nothing
  InterludeStepPart n _ _ -> InterludeStep n Nothing
  UpgradeDeckStep c -> normalizedCampaignStep c
  EpilogueStep -> EpilogueStep
  EpilogueStepPart _ -> EpilogueStep
  InvestigatorCampaignStep _ c -> normalizedCampaignStep c
  ResupplyPoint -> ResupplyPoint
  CheckpointStep n -> CheckpointStep n
  CampaignSpecificStep t -> CampaignSpecificStep t
