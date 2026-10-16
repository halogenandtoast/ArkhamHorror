{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.AndThenThereWereNone.Content where

import Arkham.Homebrew.AndThenThereWereNone.CardEntries ()
import Arkham.Homebrew.AndThenThereWereNone.Scenarios.AndThenThereWereNone (
  andThenThereWereNone,
 )
import Arkham.Homebrew.AndThenThereWereNone.Sets
import Arkham.Homebrew.Import

scenarios :: HomebrewScenarios
scenarios =
  [
    ( ":and-then-there-were-none:001"
    , HomebrewScenario AndThenThereWereNone andThenThereWereNone
    )
  ]

data AndThenThereWereNoneContent

instance IsHomebrewContent AndThenThereWereNoneContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
