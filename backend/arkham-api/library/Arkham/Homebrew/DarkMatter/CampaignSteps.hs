module Arkham.Homebrew.DarkMatter.CampaignSteps where

import Arkham.CampaignStep
import Arkham.Prelude

pattern TheTatterdemalion :: CampaignStep
pattern TheTatterdemalion <- ScenarioStep ":dark-matter:013"
  where
    TheTatterdemalion = ScenarioStep ":dark-matter:013"

pattern ElectricNightmare :: CampaignStep
pattern ElectricNightmare <- ScenarioStep ":dark-matter:053"
  where
    ElectricNightmare = ScenarioStep ":dark-matter:053"

pattern LostQuantum :: CampaignStep
pattern LostQuantum <- ScenarioStep ":dark-matter:087"
  where
    LostQuantum = ScenarioStep ":dark-matter:087"

pattern InTheShadowOfEarth :: CampaignStep
pattern InTheShadowOfEarth <- ScenarioStep ":dark-matter:112"
  where
    InTheShadowOfEarth = ScenarioStep ":dark-matter:112"

pattern StrangeMoons :: CampaignStep
pattern StrangeMoons <- ScenarioStep ":dark-matter:153"
  where
    StrangeMoons = ScenarioStep ":dark-matter:153"

pattern TheMachineInYellow :: CampaignStep
pattern TheMachineInYellow <- ScenarioStep ":dark-matter:190"
  where
    TheMachineInYellow = ScenarioStep ":dark-matter:190"

pattern FragmentOfCarcosa :: CampaignStep
pattern FragmentOfCarcosa <- ScenarioStep ":dark-matter:209"
  where
    FragmentOfCarcosa = ScenarioStep ":dark-matter:209"

pattern Starfall :: CampaignStep
pattern Starfall <- ScenarioStep ":dark-matter:243"
  where
    Starfall = ScenarioStep ":dark-matter:243"

-- Interludes
pattern MissionBriefing :: CampaignStep
pattern MissionBriefing = InterludeStep 1 Nothing

-- | The recurring "The Search for Fragment" choice (guide p10). Reached after
-- each Scenario III until all three traces are done.
pattern TheSearchForFragment :: CampaignStep
pattern TheSearchForFragment = InterludeStep 2 Nothing

pattern Introspection :: CampaignStep
pattern Introspection = InterludeStep 3 Nothing
