{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheMasqueOfTheRedDeath.Defs (module Arkham.Homebrew.TheMasqueOfTheRedDeath.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefEntries ()
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Traits qualified as Traits

data TheMasqueOfTheRedDeathDefs

instance IsHomebrewDefs TheMasqueOfTheRedDeathDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
