{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.NightOfVespers.Defs where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.NightOfVespers.CardDefEntries ()
import Arkham.Trait (Trait (HomebrewTrait))

data NightOfVespersDefs

instance IsHomebrewDefs NightOfVespersDefs where
  homebrewDefs = (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = [HomebrewTrait "O'Bannion"]}
