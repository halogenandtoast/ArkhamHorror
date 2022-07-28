module Arkham.Campaigns.TheForgottenAge.CampaignSteps where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep

nextStep :: CampaignAttrs -> Maybe CampaignStep
nextStep a = case campaignStep a of
  Just PrologueStep -> Just (ScenarioStep "04043")
  Just (ScenarioStep "04043") -> Just (UpgradeDeckStep $ InterludeStep 1 Nothing)
  Just (InterludeStep 1 _) -> Just (ScenarioStep "04054")
  Just (ScenarioStep "04054") -> Just (UpgradeDeckStep $ InterludeStep 2 Nothing)
  Just (InterludeStep 2 _) -> Just (ScenarioStep "04113")
  -- resupply
  Just (ScenarioStep "04113") -> Just (UpgradeDeckStep $ ScenarioStep "04161")
  Just (ScenarioStep "04161") -> Just (UpgradeDeckStep $ InterludeStep 3 Nothing)
  Just (InterludeStep 3 _) -> Just (ScenarioStepPart "04205" 1)
  Just (ScenarioStepPart "04205" 1) -> Just (ScenarioStepPart "04205" 2)
  Just (ScenarioStepPart "04205" 2) -> Just (UpgradeDeckStep $ ScenarioStep "04237")
  Just (ScenarioStep "04237") -> Just (UpgradeDeckStep $ InterludeStep 4 Nothing)
  Just (InterludeStep 4 _) -> Just (ScenarioStep "04277")
  Just (ScenarioStep "04277") -> Just (UpgradeDeckStep $ InterludeStep 5 Nothing)
  Just (InterludeStep 5 _) -> Just (ScenarioStep "04314")
  Just (ScenarioStep "04314") -> Just EpilogueStep
  Just (UpgradeDeckStep nextStep') -> Just nextStep'
  _ -> Nothing
