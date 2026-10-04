{- | The campaign's steps are its own scenario reference cards, so that a saved
Return to game never resolves to an official The Innsmouth Conspiracy scenario.
Interlude, prologue and epilogue steps are the official campaign's and are not
redeclared here.
-}
module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CampaignSteps where

import Arkham.CampaignStep

pattern ReturnToThePitOfDespair :: CampaignStep
pattern ReturnToThePitOfDespair <- ScenarioStep ":return-to-the-innsmouth-conspiracy:018"
  where
    ReturnToThePitOfDespair = ScenarioStep ":return-to-the-innsmouth-conspiracy:018"

pattern ReturnToTheVanishingOfElinaHarper :: CampaignStep
pattern ReturnToTheVanishingOfElinaHarper <- ScenarioStep ":return-to-the-innsmouth-conspiracy:022"
  where
    ReturnToTheVanishingOfElinaHarper = ScenarioStep ":return-to-the-innsmouth-conspiracy:022"

pattern ReturnToInTooDeep :: CampaignStep
pattern ReturnToInTooDeep <- ScenarioStep ":return-to-the-innsmouth-conspiracy:028"
  where
    ReturnToInTooDeep = ScenarioStep ":return-to-the-innsmouth-conspiracy:028"

pattern ReturnToDevilReef :: CampaignStep
pattern ReturnToDevilReef <- ScenarioStep ":return-to-the-innsmouth-conspiracy:031"
  where
    ReturnToDevilReef = ScenarioStep ":return-to-the-innsmouth-conspiracy:031"

pattern ReturnToHorrorInHighGear :: CampaignStep
pattern ReturnToHorrorInHighGear <- ScenarioStep ":return-to-the-innsmouth-conspiracy:035"
  where
    ReturnToHorrorInHighGear = ScenarioStep ":return-to-the-innsmouth-conspiracy:035"

pattern ReturnToALightInTheFog :: CampaignStep
pattern ReturnToALightInTheFog <- ScenarioStep ":return-to-the-innsmouth-conspiracy:039"
  where
    ReturnToALightInTheFog = ScenarioStep ":return-to-the-innsmouth-conspiracy:039"

pattern ReturnToTheLairOfDagon :: CampaignStep
pattern ReturnToTheLairOfDagon <- ScenarioStep ":return-to-the-innsmouth-conspiracy:043"
  where
    ReturnToTheLairOfDagon = ScenarioStep ":return-to-the-innsmouth-conspiracy:043"

pattern ReturnToIntoTheMaelstrom :: CampaignStep
pattern ReturnToIntoTheMaelstrom <- ScenarioStep ":return-to-the-innsmouth-conspiracy:048"
  where
    ReturnToIntoTheMaelstrom = ScenarioStep ":return-to-the-innsmouth-conspiracy:048"
