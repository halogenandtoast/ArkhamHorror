{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ConsternationOnTheConstellation.Content where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardEntries ()
import Arkham.Homebrew.ConsternationOnTheConstellation.Scenarios.ConsternationOnTheConstellation (
  consternationOnTheConstellation,
 )
import Arkham.Homebrew.ConsternationOnTheConstellation.Sets
import Arkham.Homebrew.Import

{- | A standalone side story: one scenario, no campaign of its own. It records
into whichever campaign log is running.
-}
scenarios :: HomebrewScenarios
scenarios =
  [
    ( ":consternation-on-the-constellation:001"
    , HomebrewScenario ConsternationOnTheConstellation consternationOnTheConstellation
    )
  ]

data ConsternationOnTheConstellationContent

instance IsHomebrewContent ConsternationOnTheConstellationContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
