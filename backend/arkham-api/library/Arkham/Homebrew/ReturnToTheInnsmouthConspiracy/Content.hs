{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Campaign (returnToTheInnsmouthConspiracy)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardEntries ()
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToALightInTheFog (
  returnToALightInTheFog,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToDevilReef (
  returnToDevilReef,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToHorrorInHighGear (
  returnToHorrorInHighGear,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToInTooDeep (
  returnToInTooDeep,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToIntoTheMaelstrom (
  returnToIntoTheMaelstrom,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToTheLairOfDagon (
  returnToTheLairOfDagon,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToThePitOfDespair (
  returnToThePitOfDespair,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToTheVanishingOfElinaHarper (
  returnToTheVanishingOfElinaHarper,
 )
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets

{- | All eight "Return to <scenario>" reference cards. The encounter set each is keyed to
is this box's own, while the scenario it wraps stays the official one.
-}
scenarios :: HomebrewScenarios
scenarios =
  [
    ( ":return-to-the-innsmouth-conspiracy:018"
    , HomebrewScenario ReturnToThePitOfDespair returnToThePitOfDespair
    )
  ,
    ( ":return-to-the-innsmouth-conspiracy:022"
    , HomebrewScenario ReturnToTheVanishingOfElinaHarper returnToTheVanishingOfElinaHarper
    )
  , (":return-to-the-innsmouth-conspiracy:028", HomebrewScenario ReturnToInTooDeep returnToInTooDeep)
  , (":return-to-the-innsmouth-conspiracy:031", HomebrewScenario ReturnToDevilReef returnToDevilReef)
  ,
    ( ":return-to-the-innsmouth-conspiracy:035"
    , HomebrewScenario ReturnToHorrorInHighGear returnToHorrorInHighGear
    )
  ,
    ( ":return-to-the-innsmouth-conspiracy:039"
    , HomebrewScenario ReturnToALightInTheFog returnToALightInTheFog
    )
  ,
    ( ":return-to-the-innsmouth-conspiracy:043"
    , HomebrewScenario ReturnToTheLairOfDagon returnToTheLairOfDagon
    )
  ,
    ( ":return-to-the-innsmouth-conspiracy:048"
    , HomebrewScenario ReturnToIntoTheMaelstrom returnToIntoTheMaelstrom
    )
  ]

campaigns :: HomebrewCampaigns
campaigns = [(":return-to-the-innsmouth-conspiracy", HomebrewCampaign returnToTheInnsmouthConspiracy)]

data ReturnToTheInnsmouthConspiracyContent

instance IsHomebrewContent ReturnToTheInnsmouthConspiracyContent where
  homebrewContent =
    $(generateHomebrew)
      { scenarios = scenarios
      , campaigns = campaigns
      }
