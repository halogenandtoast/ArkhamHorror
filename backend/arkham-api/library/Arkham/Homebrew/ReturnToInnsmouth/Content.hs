{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ReturnToInnsmouth.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.ReturnToInnsmouth.Campaign (returnToInnsmouth)
import Arkham.Homebrew.ReturnToInnsmouth.CardEntries ()
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToALightInTheFog (returnToALightInTheFog)
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToDevilReef (returnToDevilReef)
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToHorrorInHighGear (
  returnToHorrorInHighGear,
 )
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToInTooDeep (returnToInTooDeep)
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToIntoTheMaelstrom (
  returnToIntoTheMaelstrom,
 )
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToTheLairOfDagon (returnToTheLairOfDagon)
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToThePitOfDespair (returnToThePitOfDespair)
import Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToTheVanishingOfElinaHarper (
  returnToTheVanishingOfElinaHarper,
 )
import Arkham.Homebrew.ReturnToInnsmouth.Sets

{- | All eight "Return to <scenario>" reference cards. The encounter set each is keyed to
is this box's own, while the scenario it wraps stays the official one.
-}
scenarios :: HomebrewScenarios
scenarios =
  [ (":return-to-innsmouth:018", HomebrewScenario ReturnToThePitOfDespair returnToThePitOfDespair)
  ,
    ( ":return-to-innsmouth:022"
    , HomebrewScenario ReturnToTheVanishingOfElinaHarper returnToTheVanishingOfElinaHarper
    )
  , (":return-to-innsmouth:028", HomebrewScenario ReturnToInTooDeep returnToInTooDeep)
  , (":return-to-innsmouth:031", HomebrewScenario ReturnToDevilReef returnToDevilReef)
  ,
    ( ":return-to-innsmouth:035"
    , HomebrewScenario ReturnToHorrorInHighGear returnToHorrorInHighGear
    )
  , (":return-to-innsmouth:039", HomebrewScenario ReturnToALightInTheFog returnToALightInTheFog)
  , (":return-to-innsmouth:043", HomebrewScenario ReturnToTheLairOfDagon returnToTheLairOfDagon)
  ,
    ( ":return-to-innsmouth:048"
    , HomebrewScenario ReturnToIntoTheMaelstrom returnToIntoTheMaelstrom
    )
  ]

campaigns :: HomebrewCampaigns
campaigns = [(":return-to-innsmouth", HomebrewCampaign returnToInnsmouth)]

data ReturnToInnsmouthContent

instance IsHomebrewContent ReturnToInnsmouthContent where
  homebrewContent =
    $(generateHomebrew)
      { scenarios = scenarios
      , campaigns = campaigns
      }
