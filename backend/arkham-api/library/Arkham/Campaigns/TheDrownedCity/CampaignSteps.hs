module Arkham.Campaigns.TheDrownedCity.CampaignSteps where

import Arkham.CampaignStep
import Arkham.Prelude

pattern OneLastJob :: CampaignStep
pattern OneLastJob <- ScenarioStep "11501"
  where
    OneLastJob = ScenarioStep "11501"

pattern TheWesternWall :: CampaignStep
pattern TheWesternWall <- ScenarioStep "11517"
  where
    TheWesternWall = ScenarioStep "11517"

pattern TheDrownedQuarter :: CampaignStep
pattern TheDrownedQuarter <- ScenarioStep "11536"
  where
    TheDrownedQuarter = ScenarioStep "11536"

pattern TheApiary :: CampaignStep
pattern TheApiary <- ScenarioStep "11553"
  where
    TheApiary = ScenarioStep "11553"

pattern TheGrandVault :: CampaignStep
pattern TheGrandVault <- ScenarioStep "11587"
  where
    TheGrandVault = ScenarioStep "11587"

pattern CourtOfTheAncients :: CampaignStep
pattern CourtOfTheAncients <- ScenarioStep "11612"
  where
    CourtOfTheAncients = ScenarioStep "11612"

pattern ObsidianCanyons :: CampaignStep
pattern ObsidianCanyons <- ScenarioStep "11639"
  where
    ObsidianCanyons = ScenarioStep "11639"

pattern SepulchreOfTheSleeper :: CampaignStep
pattern SepulchreOfTheSleeper <- ScenarioStep "11673"
  where
    SepulchreOfTheSleeper = ScenarioStep "11673"

pattern TheDoomOfArkhamPartI :: CampaignStep
pattern TheDoomOfArkhamPartI <- ScenarioStep "11682"
  where
    TheDoomOfArkhamPartI = ScenarioStep "11682"

pattern TheDoomOfArkhamPartII :: CampaignStep
pattern TheDoomOfArkhamPartII <- ScenarioStep "11688a"
  where
    TheDoomOfArkhamPartII = ScenarioStep "11688a"

-- Interludes
pattern AnOfferYouCantRefuse :: CampaignStep
pattern AnOfferYouCantRefuse = InterludeStep 1 Nothing

pattern ExpeditionToRlyeh :: CampaignStep
pattern ExpeditionToRlyeh = InterludeStep 2 Nothing

pattern TheAwakening :: CampaignStep
pattern TheAwakening = InterludeStep 3 Nothing

pattern ReturnToArkham :: CampaignStep
pattern ReturnToArkham = InterludeStep 4 Nothing
