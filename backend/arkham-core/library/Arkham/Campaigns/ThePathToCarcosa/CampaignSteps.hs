module Arkham.Campaigns.ThePathToCarcosa.CampaignSteps where

import Arkham.Prelude

import Arkham.Types.Campaign.Attrs
import Arkham.Types.CampaignStep

nextStep :: CampaignAttrs -> Maybe CampaignStep
nextStep a = case campaignStep a of
  Just PrologueStep -> Just (ScenarioStep "03043")
  Just (ScenarioStep "03043") -> Just (UpgradeDeckStep $ ScenarioStep "03061")
  Just (ScenarioStep "03061") -> Just (UpgradeDeckStep $ ScenarioStep "03120")
  Just (InterludeStep 1 _) -> Just (UpgradeDeckStep $ ScenarioStep "03120")
  _ -> Nothing
