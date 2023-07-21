module Arkham.Campaigns.NightOfTheZealot.CampaignSteps where

import Arkham.CampaignStep

pattern TheGathering :: CampaignStep
pattern TheGathering <- ScenarioStep "01104"
  where
    TheGathering = ScenarioStep "01104"

pattern TheMidnightMasks :: CampaignStep
pattern TheMidnightMasks <- ScenarioStep "01120"
  where
    TheMidnightMasks = ScenarioStep "01120"

pattern TheDevourerBelow :: CampaignStep
pattern TheDevourerBelow <- ScenarioStep "01142"
  where
    TheDevourerBelow = ScenarioStep "01142"

pattern ReturnToTheGathering :: CampaignStep
pattern ReturnToTheGathering <- ScenarioStep "50011"
  where
    ReturnToTheGathering = ScenarioStep "50011"

pattern ReturnToTheMidnightMasks :: CampaignStep
pattern ReturnToTheMidnightMasks <- ScenarioStep "50025"
  where
    ReturnToTheMidnightMasks = ScenarioStep "50025"

pattern ReturnToTheDevourerBelow :: CampaignStep
pattern ReturnToTheDevourerBelow <- ScenarioStep "50032"
  where
    ReturnToTheDevourerBelow = ScenarioStep "50032"
