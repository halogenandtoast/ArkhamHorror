module Arkham.Homebrew.DarkMatter.CampaignSteps where

import Arkham.CampaignStep
import Arkham.Prelude

pattern TheTatterdemalion :: CampaignStep
pattern TheTatterdemalion <- ScenarioStep "z-dark-matter-013"
  where
    TheTatterdemalion = ScenarioStep "z-dark-matter-013"

pattern ElectricNightmare :: CampaignStep
pattern ElectricNightmare <- ScenarioStep "z-dark-matter-053"
  where
    ElectricNightmare = ScenarioStep "z-dark-matter-053"

pattern LostQuantum :: CampaignStep
pattern LostQuantum <- ScenarioStep "z-dark-matter-090"
  where
    LostQuantum = ScenarioStep "z-dark-matter-090"

pattern InTheShadowOfEarth :: CampaignStep
pattern InTheShadowOfEarth <- ScenarioStep "z-dark-matter-115"
  where
    InTheShadowOfEarth = ScenarioStep "z-dark-matter-115"

pattern StrangeMoons :: CampaignStep
pattern StrangeMoons <- ScenarioStep "z-dark-matter-156"
  where
    StrangeMoons = ScenarioStep "z-dark-matter-156"

pattern TheMachineInYellow :: CampaignStep
pattern TheMachineInYellow <- ScenarioStep "z-dark-matter-193"
  where
    TheMachineInYellow = ScenarioStep "z-dark-matter-193"

pattern FragmentOfCarcosa :: CampaignStep
pattern FragmentOfCarcosa <- ScenarioStep "z-dark-matter-212"
  where
    FragmentOfCarcosa = ScenarioStep "z-dark-matter-212"

pattern Starfall :: CampaignStep
pattern Starfall <- ScenarioStep "z-dark-matter-246"
  where
    Starfall = ScenarioStep "z-dark-matter-246"

-- Interludes
pattern MissionBriefing :: CampaignStep
pattern MissionBriefing = InterludeStep 1 Nothing

-- | The recurring "The Search for Fragment" choice (guide p10). Reached after
-- each Scenario III until all three traces are done.
pattern TheSearchForFragment :: CampaignStep
pattern TheSearchForFragment = InterludeStep 2 Nothing

pattern Introspection :: CampaignStep
pattern Introspection = InterludeStep 3 Nothing
