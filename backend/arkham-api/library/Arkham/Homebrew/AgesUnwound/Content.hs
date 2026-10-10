{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.AgesUnwound.Content where

import Arkham.Homebrew.AgesUnwound.Campaign (agesUnwound)
import Arkham.Homebrew.AgesUnwound.CardEntries ()
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown (aWorldTornDown)
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain (aWorldTornDownAgain)
import Arkham.Homebrew.AgesUnwound.Scenarios.AYearToPlan (aYearToPlan)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire (nightOfFire)
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman (theMyriadGentleman)
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut (timeRunsOut)
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck (unstuck)
import Arkham.Homebrew.AgesUnwound.Sets
import Arkham.Homebrew.Import

scenarios :: HomebrewScenarios
scenarios =
  [ (":ages-unwound:001", HomebrewScenario NightOfFire nightOfFire)
  , (":ages-unwound:023", HomebrewScenario TheMyriadGentleman theMyriadGentleman)
  , (":ages-unwound:048", HomebrewScenario AWorldTornDown aWorldTornDown)
  , (":ages-unwound:062", HomebrewScenario Unstuck unstuck)
  , (":ages-unwound:105", HomebrewScenario AYearToPlan aYearToPlan)
  , (":ages-unwound:155", HomebrewScenario AWorldTornDownAgain aWorldTornDownAgain)
  , (":ages-unwound:182", HomebrewScenario TimeRunsOut timeRunsOut)
  ]

campaigns :: HomebrewCampaigns
campaigns = [(":ages-unwound", HomebrewCampaign agesUnwound)]

data AgesUnwoundContent

instance IsHomebrewContent AgesUnwoundContent where
  homebrewContent =
    $(generateHomebrew)
      { scenarios = scenarios
      , campaigns = campaigns
      }
