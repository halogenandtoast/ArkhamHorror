{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardEntries ()
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Scenarios.TheFallOfTheHouseOfUsher (
  theFallOfTheHouseOfUsher,
 )
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Sets

scenarios :: HomebrewScenarios
scenarios =
  [
    ( ":the-fall-of-the-house-of-usher:001"
    , HomebrewScenario TheFallOfTheHouseOfUsher theFallOfTheHouseOfUsher
    )
  ]

data TheFallOfTheHouseOfUsherContent

instance IsHomebrewContent TheFallOfTheHouseOfUsherContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
