{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.NightOfVespers.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.NightOfVespers.Campaign (nightOfVespers)
import Arkham.Homebrew.NightOfVespers.CardEntries ()
import Arkham.Homebrew.NightOfVespers.Scenarios.AChorusOfSuffering (aChorusOfSuffering)
import Arkham.Homebrew.NightOfVespers.Scenarios.BehindClosedDoors (behindClosedDoors)
import Arkham.Homebrew.NightOfVespers.Scenarios.DancingWithDeath (dancingWithDeath)
import Arkham.Homebrew.NightOfVespers.Scenarios.MothersMercy (mothersMercy)
import Arkham.Homebrew.NightOfVespers.Scenarios.TheCityOfFog (theCityOfFog)
import Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons (theEndOfSeasons)
import Arkham.Homebrew.NightOfVespers.Scenarios.TheGardenOfBones (theGardenOfBones)
import Arkham.Homebrew.NightOfVespers.Scenarios.TheTeachingsOfAradia (theTeachingsOfAradia)
import Arkham.Homebrew.NightOfVespers.Sets

scenarios :: HomebrewScenarios
scenarios =
  [ (":night-of-vespers:001", HomebrewScenario TheEndOfSeasons theEndOfSeasons)
  , (":night-of-vespers:037", HomebrewScenario BehindClosedDoors behindClosedDoors)
  , (":night-of-vespers:075", HomebrewScenario DancingWithDeath dancingWithDeath)
  , (":night-of-vespers:105", HomebrewScenario TheTeachingsOfAradia theTeachingsOfAradia)
  , (":night-of-vespers:136", HomebrewScenario TheCityOfFog theCityOfFog)
  , (":night-of-vespers:168", HomebrewScenario TheGardenOfBones theGardenOfBones)
  , (":night-of-vespers:214", HomebrewScenario AChorusOfSuffering aChorusOfSuffering)
  , (":night-of-vespers:257", HomebrewScenario MothersMercy mothersMercy)
  ]

campaigns :: HomebrewCampaigns
campaigns = [(":night-of-vespers", HomebrewCampaign nightOfVespers)]

data NightOfVespersContent

instance IsHomebrewContent NightOfVespersContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios, campaigns = campaigns}
