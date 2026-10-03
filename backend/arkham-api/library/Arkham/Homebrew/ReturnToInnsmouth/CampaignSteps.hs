{- | The campaign's steps are its own scenario reference cards, so that a saved
Return to game never resolves to an official The Innsmouth Conspiracy scenario.
Interlude, prologue and epilogue steps are the official campaign's and are not
redeclared here.
-}
module Arkham.Homebrew.ReturnToInnsmouth.CampaignSteps where

import Arkham.CampaignStep

pattern ReturnToThePitOfDespair :: CampaignStep
pattern ReturnToThePitOfDespair <- ScenarioStep ":return-to-innsmouth:018"
  where
    ReturnToThePitOfDespair = ScenarioStep ":return-to-innsmouth:018"

pattern ReturnToTheVanishingOfElinaHarper :: CampaignStep
pattern ReturnToTheVanishingOfElinaHarper <- ScenarioStep ":return-to-innsmouth:022"
  where
    ReturnToTheVanishingOfElinaHarper = ScenarioStep ":return-to-innsmouth:022"

pattern ReturnToInTooDeep :: CampaignStep
pattern ReturnToInTooDeep <- ScenarioStep ":return-to-innsmouth:028"
  where
    ReturnToInTooDeep = ScenarioStep ":return-to-innsmouth:028"

pattern ReturnToDevilReef :: CampaignStep
pattern ReturnToDevilReef <- ScenarioStep ":return-to-innsmouth:031"
  where
    ReturnToDevilReef = ScenarioStep ":return-to-innsmouth:031"

pattern ReturnToHorrorInHighGear :: CampaignStep
pattern ReturnToHorrorInHighGear <- ScenarioStep ":return-to-innsmouth:035"
  where
    ReturnToHorrorInHighGear = ScenarioStep ":return-to-innsmouth:035"

pattern ReturnToALightInTheFog :: CampaignStep
pattern ReturnToALightInTheFog <- ScenarioStep ":return-to-innsmouth:039"
  where
    ReturnToALightInTheFog = ScenarioStep ":return-to-innsmouth:039"

pattern ReturnToTheLairOfDagon :: CampaignStep
pattern ReturnToTheLairOfDagon <- ScenarioStep ":return-to-innsmouth:043"
  where
    ReturnToTheLairOfDagon = ScenarioStep ":return-to-innsmouth:043"

pattern ReturnToIntoTheMaelstrom :: CampaignStep
pattern ReturnToIntoTheMaelstrom <- ScenarioStep ":return-to-innsmouth:048"
  where
    ReturnToIntoTheMaelstrom = ScenarioStep ":return-to-innsmouth:048"
