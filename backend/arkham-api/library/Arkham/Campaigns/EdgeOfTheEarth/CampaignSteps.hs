module Arkham.Campaigns.EdgeOfTheEarth.CampaignSteps where

import Arkham.CampaignStep

pattern IceAndDeathPart1 :: CampaignStep
pattern IceAndDeathPart1 <- ScenarioStep "08501a"
  where
    IceAndDeathPart1 = ScenarioStep "08501a"

pattern IceAndDeathPart2 :: CampaignStep
pattern IceAndDeathPart2 <- ScenarioStep "08501b"
  where
    IceAndDeathPart2 = ScenarioStep "08501b"

pattern IceAndDeathPart3 :: CampaignStep
pattern IceAndDeathPart3 <- ScenarioStep "08501c"
  where
    IceAndDeathPart3 = ScenarioStep "08501c"

pattern FatalMirage :: CampaignStep
pattern FatalMirage <- ScenarioStep "08549"
  where
    FatalMirage = ScenarioStep "08549"

pattern ToTheForbiddenPeaks :: CampaignStep
pattern ToTheForbiddenPeaks <- ScenarioStep "08549"
  where
    ToTheForbiddenPeaks = ScenarioStep "08596"

pattern CityOfTheElderThings :: CampaignStep
pattern CityOfTheElderThings <- ScenarioStep "08621"
  where
    CityOfTheElderThings = ScenarioStep "08621"

pattern TheHeartOfMadness :: CampaignStep
pattern TheHeartOfMadness <- ScenarioStep "08648"
  where
    TheHeartOfMadness = ScenarioStep "08648"
