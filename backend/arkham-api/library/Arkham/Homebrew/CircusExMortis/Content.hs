{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.CircusExMortis.Content where

import Arkham.Homebrew.CircusExMortis.Campaign (circusExMortis)
import Arkham.Homebrew.CircusExMortis.CardEntries ()
import Arkham.Homebrew.CircusExMortis.Scenarios.AllPointsWest (allPointsWest)
import Arkham.Homebrew.CircusExMortis.Scenarios.Bacchanalia (bacchanalia)
import Arkham.Homebrew.CircusExMortis.Scenarios.HarmsWay (harmsWay)
import Arkham.Homebrew.CircusExMortis.Scenarios.OneNightOnly (oneNightOnly)
import Arkham.Homebrew.CircusExMortis.Scenarios.PiperAtTheGatesOfDawn (piperAtTheGatesOfDawn)
import Arkham.Homebrew.CircusExMortis.Scenarios.RedSunrise (redSunrise)
import Arkham.Homebrew.CircusExMortis.Scenarios.ThePrimrosePath (thePrimrosePath)
import Arkham.Homebrew.CircusExMortis.Scenarios.ThousandToOne (thousandToOne)
import Arkham.Homebrew.CircusExMortis.Sets
import Arkham.Homebrew.Import

scenarios :: HomebrewScenarios
scenarios =
  [ (":circus-ex-mortis:001", HomebrewScenario OneNightOnly oneNightOnly)
  , (":circus-ex-mortis:017", HomebrewScenario ThePrimrosePath thePrimrosePath)
  , (":circus-ex-mortis:040", HomebrewScenario HarmsWay harmsWay)
  , (":circus-ex-mortis:074", HomebrewScenario AllPointsWest allPointsWest)
  , (":circus-ex-mortis:108", HomebrewScenario PiperAtTheGatesOfDawn piperAtTheGatesOfDawn)
  , (":circus-ex-mortis:122", HomebrewScenario Bacchanalia bacchanalia)
  , (":circus-ex-mortis:153", HomebrewScenario RedSunrise redSunrise)
  , (":circus-ex-mortis:190", HomebrewScenario ThousandToOne thousandToOne)
  ]

campaigns :: HomebrewCampaigns
campaigns = [(":circus-ex-mortis", HomebrewCampaign circusExMortis)]

data CircusExMortisContent

instance IsHomebrewContent CircusExMortisContent where
  homebrewContent =
    $(generateHomebrew)
      { scenarios = scenarios
      , campaigns = campaigns
      }
