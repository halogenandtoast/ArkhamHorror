module Arkham.Campaigns.CircusExMortis.CampaignSteps where

import Arkham.CampaignStep
import Arkham.Prelude

pattern OneNightOnly :: CampaignStep
pattern OneNightOnly <- ScenarioStep "z-circus-ex-mortis-001"
  where
    OneNightOnly = ScenarioStep "z-circus-ex-mortis-001"

pattern ThePrimrosePath :: CampaignStep
pattern ThePrimrosePath <- ScenarioStep "z-circus-ex-mortis-017"
  where
    ThePrimrosePath = ScenarioStep "z-circus-ex-mortis-017"

pattern HarmsWay :: CampaignStep
pattern HarmsWay <- ScenarioStep "z-circus-ex-mortis-042"
  where
    HarmsWay = ScenarioStep "z-circus-ex-mortis-042"

pattern AllPointsWest :: CampaignStep
pattern AllPointsWest <- ScenarioStep "z-circus-ex-mortis-076"
  where
    AllPointsWest = ScenarioStep "z-circus-ex-mortis-076"

pattern PiperAtTheGatesOfDawn :: CampaignStep
pattern PiperAtTheGatesOfDawn <- ScenarioStep "z-circus-ex-mortis-110"
  where
    PiperAtTheGatesOfDawn = ScenarioStep "z-circus-ex-mortis-110"

pattern Bacchanalia :: CampaignStep
pattern Bacchanalia <- ScenarioStep "z-circus-ex-mortis-124"
  where
    Bacchanalia = ScenarioStep "z-circus-ex-mortis-124"

pattern RedSunrise :: CampaignStep
pattern RedSunrise <- ScenarioStep "z-circus-ex-mortis-155"
  where
    RedSunrise = ScenarioStep "z-circus-ex-mortis-155"

pattern ThousandToOne :: CampaignStep
pattern ThousandToOne <- ScenarioStep "z-circus-ex-mortis-192"
  where
    ThousandToOne = ScenarioStep "z-circus-ex-mortis-192"

-- Interludes
pattern TheFutureAndThePast :: CampaignStep
pattern TheFutureAndThePast = InterludeStep 1 Nothing

pattern WrittenInStone :: CampaignStep
pattern WrittenInStone = InterludeStep 2 Nothing

pattern GoodOmens :: CampaignStep
pattern GoodOmens = InterludeStep 3 Nothing
