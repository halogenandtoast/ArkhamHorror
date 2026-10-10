{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheMasqueOfTheRedDeath.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardEntries ()
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Scenarios.TheMasqueOfTheRedDeath (
  theMasqueOfTheRedDeath,
 )
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets

{- | A standalone side story: one scenario, no campaign of its own. It records
into whichever campaign log is running.
-}
scenarios :: HomebrewScenarios
scenarios =
  [ (":the-masque-of-the-red-death:001", HomebrewScenario TheMasqueOfTheRedDeath theMasqueOfTheRedDeath)
  ]

data TheMasqueOfTheRedDeathContent

instance IsHomebrewContent TheMasqueOfTheRedDeathContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
