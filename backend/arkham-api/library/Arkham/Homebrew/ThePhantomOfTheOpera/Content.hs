{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ThePhantomOfTheOpera.Content where

import Arkham.Homebrew.Import
import Arkham.Homebrew.ThePhantomOfTheOpera.CardEntries ()
import Arkham.Homebrew.ThePhantomOfTheOpera.Scenarios.ThePhantomOfTheOpera (thePhantomOfTheOpera)
import Arkham.Homebrew.ThePhantomOfTheOpera.Sets

scenarios :: HomebrewScenarios
scenarios =
  [ (":the-phantom-of-the-opera:001", HomebrewScenario ThePhantomOfTheOpera thePhantomOfTheOpera)
  ]

data ThePhantomOfTheOperaContent

instance IsHomebrewContent ThePhantomOfTheOperaContent where
  homebrewContent = $(generateHomebrew) {scenarios = scenarios}
