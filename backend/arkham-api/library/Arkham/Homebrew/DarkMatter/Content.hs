{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.DarkMatter.Content where

import Arkham.Homebrew.DarkMatter.Campaign (darkMatter)
import Arkham.Homebrew.DarkMatter.CardEntries ()
import Arkham.Homebrew.DarkMatter.Scenarios.ElectricNightmare (electricNightmare)
import Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa (fragmentOfCarcosa)
import Arkham.Homebrew.DarkMatter.Scenarios.InTheShadowOfEarth (inTheShadowOfEarth)
import Arkham.Homebrew.DarkMatter.Scenarios.LostQuantum (lostQuantum)
import Arkham.Homebrew.DarkMatter.Scenarios.Starfall (starfall)
import Arkham.Homebrew.DarkMatter.Scenarios.StrangeMoons (strangeMoons)
import Arkham.Homebrew.DarkMatter.Scenarios.TheMachineInYellow (theMachineInYellow)
import Arkham.Homebrew.DarkMatter.Scenarios.TheTatterdemalion (theTatterdemalion)
import Arkham.Homebrew.DarkMatter.Sets
import Arkham.Homebrew.Import

scenarios :: HomebrewScenarios
scenarios =
  [ (":dark-matter:014", HomebrewScenario TheTatterdemalion theTatterdemalion)
  , (":dark-matter:054", HomebrewScenario ElectricNightmare electricNightmare)
  , (":dark-matter:089", HomebrewScenario LostQuantum lostQuantum)
  , (":dark-matter:112", HomebrewScenario InTheShadowOfEarth inTheShadowOfEarth)
  , (":dark-matter:153", HomebrewScenario StrangeMoons strangeMoons)
  , (":dark-matter:190", HomebrewScenario TheMachineInYellow theMachineInYellow)
  , (":dark-matter:209", HomebrewScenario FragmentOfCarcosa fragmentOfCarcosa)
  , (":dark-matter:243", HomebrewScenario Starfall starfall)
  ]

campaigns :: HomebrewCampaigns
campaigns = [(":dark-matter", HomebrewCampaign darkMatter)]

data DarkMatterContent

instance IsHomebrewContent DarkMatterContent where
  homebrewContent =
    $(generateHomebrew)
      { scenarios = scenarios
      , campaigns = campaigns
      }
