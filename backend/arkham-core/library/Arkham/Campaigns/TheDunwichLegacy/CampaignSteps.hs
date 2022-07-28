module Arkham.Campaigns.TheDunwichLegacy.CampaignSteps where

import Arkham.Prelude

import Arkham.Resolution
import Arkham.Campaign.Types
import Arkham.CampaignStep

nextStep :: CampaignAttrs -> Maybe CampaignStep
nextStep a = case campaignStep a of
  Just PrologueStep -> error $ "Unhandled campaign step: " <> show a
  Just (ScenarioStep "02041") ->
    if ScenarioStep "02062" `elem` campaignCompletedSteps a
      then Just $ InterludeStep 1 Nothing
      else Just (UpgradeDeckStep $ ScenarioStep "02062")
  Just (ScenarioStep "02062") ->
    if ScenarioStep "02041" `elem` campaignCompletedSteps a
      then Just $ InterludeStep 1 Nothing
      else Just (UpgradeDeckStep $ ScenarioStep "02041")
  Just (InterludeStep 1 _) ->
    Just (UpgradeDeckStep $ ScenarioStep "02118")
  Just (ScenarioStep "02118") ->
    Just (UpgradeDeckStep $ ScenarioStep "02159")
  Just (ScenarioStep "02159") ->
    Just (UpgradeDeckStep $ ScenarioStep "02195")
  Just (ScenarioStep "02195") ->
    case lookup "02195" (campaignResolutions a) of
      Just NoResolution ->
        Just (UpgradeDeckStep $ ScenarioStep "02236")
      _ -> Just $ InterludeStep 2 Nothing
  Just (InterludeStep 2 _) ->
    Just (UpgradeDeckStep $ ScenarioStep "02236")
  Just (ScenarioStep "02236") ->
    Just (UpgradeDeckStep $ ScenarioStep "02274")
  Just (ScenarioStep "02274") ->
    Just (UpgradeDeckStep $ ScenarioStep "02311")
  Just (ScenarioStep "02311") -> Nothing
  Just (UpgradeDeckStep nextStep') -> Just nextStep'
  _ -> Nothing
