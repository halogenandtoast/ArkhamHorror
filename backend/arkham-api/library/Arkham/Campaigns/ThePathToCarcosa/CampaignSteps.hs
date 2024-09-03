module Arkham.Campaigns.ThePathToCarcosa.CampaignSteps where

import Arkham.CampaignStep

pattern CurtainCall :: CampaignStep
pattern CurtainCall <- ScenarioStep "03043"
  where
    CurtainCall = ScenarioStep "03043"

pattern TheLastKing :: CampaignStep
pattern TheLastKing <- ScenarioStep "03061"
  where
    TheLastKing = ScenarioStep "03061"

pattern EchoesOfThePast :: CampaignStep
pattern EchoesOfThePast <- ScenarioStep "03120"
  where
    EchoesOfThePast = ScenarioStep "03120"

pattern TheUnspeakableOath :: CampaignStep
pattern TheUnspeakableOath <- ScenarioStep "03159"
  where
    TheUnspeakableOath = ScenarioStep "03159"

pattern APhantomOfTruth :: CampaignStep
pattern APhantomOfTruth <- ScenarioStep "03200"
  where
    APhantomOfTruth = ScenarioStep "03200"

pattern ThePallidMask :: CampaignStep
pattern ThePallidMask <- ScenarioStep "03240"
  where
    ThePallidMask = ScenarioStep "03240"

pattern BlackStarsRise :: CampaignStep
pattern BlackStarsRise <- ScenarioStep "03274"
  where
    BlackStarsRise = ScenarioStep "03274"

pattern DimCarcosa :: CampaignStep
pattern DimCarcosa <- ScenarioStep "03316"
  where
    DimCarcosa = ScenarioStep "03316"
