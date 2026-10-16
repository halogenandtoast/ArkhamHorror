{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheLegendOfSleepyHollow.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardEntries ()
import Arkham.Homebrew.TheLegendOfSleepyHollow.Scenarios.TheLegendOfSleepyHollow (
  theLegendOfSleepyHollow,
 )
import Arkham.Homebrew.TheLegendOfSleepyHollow.Sets

scenarios :: HomebrewScenarios
scenarios =
  [
    ( ":the-legend-of-sleepy-hollow:001"
    , HomebrewScenario TheLegendOfSleepyHollow theLegendOfSleepyHollow
    )
  ]

data TheLegendOfSleepyHollowContent

instance IsHomebrewContent TheLegendOfSleepyHollowContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
