module Arkham.Campaigns.NightOfTheZealot.CampaignSteps where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.CampaignStep

nextStep :: CampaignAttrs -> Maybe CampaignStep
nextStep a = case campaignStep a of
  Just PrologueStep -> Just (ScenarioStep "01104")
  Just (ScenarioStep "01104") ->
    Just (UpgradeDeckStep $ ScenarioStep "01120")
  Just (ScenarioStep "01120") ->
    Just (UpgradeDeckStep $ ScenarioStep "01142")
  Just (UpgradeDeckStep nextStep') -> Just nextStep'
  _ -> Nothing

returnToNextStep :: CampaignAttrs -> Maybe CampaignStep
returnToNextStep a = case campaignStep a of
  Just PrologueStep -> Just (ScenarioStep "50011")
  Just (ScenarioStep "50011") -> Just (UpgradeDeckStep $ ScenarioStep "50025")
  Just (ScenarioStep "50025") -> Just (UpgradeDeckStep $ ScenarioStep "50032")
  Just (UpgradeDeckStep nextStep') -> Just nextStep'
  _ -> Nothing
