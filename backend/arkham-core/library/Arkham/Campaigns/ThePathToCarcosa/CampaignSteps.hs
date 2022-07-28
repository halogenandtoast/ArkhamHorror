module Arkham.Campaigns.ThePathToCarcosa.CampaignSteps where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep

nextStep :: CampaignAttrs -> Maybe CampaignStep
nextStep a = case campaignStep a of
  Just PrologueStep -> Just (ScenarioStep "03043")
  Just (ScenarioStep "03043") -> Just (UpgradeDeckStep $ ScenarioStep "03061")
  Just (ScenarioStep "03061") -> Just (UpgradeDeckStep $ ScenarioStep "03120")
  Just (InterludeStep 1 _) -> Just (UpgradeDeckStep $ ScenarioStep "03120")
  Just (ScenarioStep "03120") -> Just (UpgradeDeckStep $ ScenarioStep "03159")
  Just (ScenarioStep "03159") -> Just (UpgradeDeckStep $ ScenarioStep "03200")
  Just (InterludeStep 2 _) -> Just (UpgradeDeckStep $ ScenarioStep "03200")
  Just (ScenarioStep "03200") -> Just (UpgradeDeckStep $ ScenarioStep "03240")
  Just (ScenarioStep "03240") -> Just (UpgradeDeckStep $ ScenarioStep "03274")
  Just (ScenarioStep "03274") -> Just (UpgradeDeckStep $ ScenarioStep "03316")
  Just (ScenarioStep "03316") -> Just EpilogueStep
  Just (UpgradeDeckStep nextStep') -> Just nextStep'
  _ -> Nothing
