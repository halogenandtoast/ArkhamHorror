{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheInvisibleMan.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.TheInvisibleMan.CardEntries ()
import Arkham.Homebrew.TheInvisibleMan.Scenarios.TheInvisibleMan (theInvisibleMan)
import Arkham.Homebrew.TheInvisibleMan.Sets

scenarios :: HomebrewScenarios
scenarios = [(":the-invisible-man:001", HomebrewScenario TheInvisibleMan theInvisibleMan)]

data TheInvisibleManContent

instance IsHomebrewContent TheInvisibleManContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
