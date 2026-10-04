{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheSymphonyOfErichZann.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.TheSymphonyOfErichZann.CardEntries ()
import Arkham.Homebrew.TheSymphonyOfErichZann.Scenarios.TheSymphonyOfErichZann (theSymphonyOfErichZann)
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets

{- | A standalone side story: one scenario, no campaign of its own. It records
into whichever campaign log is running.
-}
scenarios :: HomebrewScenarios
scenarios =
  [ (":the-symphony-of-erich-zann:001", HomebrewScenario TheSymphonyOfErichZann theSymphonyOfErichZann)
  ]

data TheSymphonyOfErichZannContent

instance IsHomebrewContent TheSymphonyOfErichZannContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
