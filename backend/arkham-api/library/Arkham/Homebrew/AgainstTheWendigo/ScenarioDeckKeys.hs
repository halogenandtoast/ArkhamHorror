{-# LANGUAGE TemplateHaskell #-}

{- | The Students' Fate deck: the three surviving story cards (one of each
named pair) the acts draw from, face down, to reveal what became of Sylvia,
Norman and Bernard.
-}
module Arkham.Homebrew.AgainstTheWendigo.ScenarioDeckKeys (module Arkham.Homebrew.AgainstTheWendigo.ScenarioDeckKeys) where

import Arkham.Homebrew.TH (declareHomebrewScenarioDeckKeys)

declareHomebrewScenarioDeckKeys ["StudentsFateDeck"]
