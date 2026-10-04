{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.AgainstTheWendigo.Content where

import Arkham.Homebrew.AgainstTheWendigo.CardEntries ()
import Arkham.Homebrew.AgainstTheWendigo.Scenarios.AgainstTheWendigo (againstTheWendigo)
import Arkham.Homebrew.AgainstTheWendigo.Sets
import Arkham.Homebrew.Import

{- | A standalone side story: one scenario, no campaign of its own. It records
into whichever campaign log is running.
-}
scenarios :: HomebrewScenarios
scenarios =
  [ (":against-the-wendigo:001", HomebrewScenario HanninahValley againstTheWendigo)
  ]

data AgainstTheWendigoContent

instance IsHomebrewContent AgainstTheWendigoContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
