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

pattern ReturnToCurtainCall :: CampaignStep
pattern ReturnToCurtainCall <- ScenarioStep "52014"
  where
    ReturnToCurtainCall = ScenarioStep "52014"

pattern ReturnToTheLastKing :: CampaignStep
pattern ReturnToTheLastKing <- ScenarioStep "52021"
  where
    ReturnToTheLastKing = ScenarioStep "52021"

pattern ReturnToEchoesOfThePast :: CampaignStep
pattern ReturnToEchoesOfThePast <- ScenarioStep "52028"
  where
    ReturnToEchoesOfThePast = ScenarioStep "52028"

pattern ReturnToTheUnspeakableOath :: CampaignStep
pattern ReturnToTheUnspeakableOath <- ScenarioStep "52034"
  where
    ReturnToTheUnspeakableOath = ScenarioStep "52034"

pattern ReturnToAPhantomOfTruth :: CampaignStep
pattern ReturnToAPhantomOfTruth <- ScenarioStep "52040"
  where
    ReturnToAPhantomOfTruth = ScenarioStep "52040"

pattern ReturnToThePallidMask :: CampaignStep
pattern ReturnToThePallidMask <- ScenarioStep "52048"
  where
    ReturnToThePallidMask = ScenarioStep "52048"

pattern ReturnToBlackStarsRise :: CampaignStep
pattern ReturnToBlackStarsRise <- ScenarioStep "52054"
  where
    ReturnToBlackStarsRise = ScenarioStep "52054"

pattern ReturnToDimCarcosa :: CampaignStep
pattern ReturnToDimCarcosa <- ScenarioStep "52059"
  where
    ReturnToDimCarcosa = ScenarioStep "52059"
