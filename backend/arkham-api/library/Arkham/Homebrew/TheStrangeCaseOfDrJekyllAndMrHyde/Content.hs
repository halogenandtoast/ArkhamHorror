{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardEntries ()
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Scenarios.TheStrangeCaseOfDrJekyllAndMrHyde (
  theStrangeCaseOfDrJekyllAndMrHyde,
 )
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets

scenarios :: HomebrewScenarios
scenarios =
  [
    ( ":the-strange-case-of-dr-jekyll-and-mr-hyde:001"
    , HomebrewScenario TheStrangeCaseOfDrJekyllAndMrHyde theStrangeCaseOfDrJekyllAndMrHyde
    )
  ]

data TheStrangeCaseOfDrJekyllAndMrHydeContent

instance IsHomebrewContent TheStrangeCaseOfDrJekyllAndMrHydeContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
