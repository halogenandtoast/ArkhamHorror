module Arkham.Campaigns.ChildrenOfBlood.CampaignSteps where

import Arkham.CampaignStep

pattern RiverOfBlood :: CampaignStep
pattern RiverOfBlood <- ScenarioStep "13001"
  where
    RiverOfBlood = ScenarioStep "13001"

pattern NewHorizons :: CampaignStep
pattern NewHorizons <- ScenarioStep "13031"
  where
    NewHorizons = ScenarioStep "13031"

pattern BloodMoney :: CampaignStep
pattern BloodMoney <- ScenarioStep "13068"
  where
    BloodMoney = ScenarioStep "13068"
