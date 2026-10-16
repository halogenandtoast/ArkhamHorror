{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Defs (module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefEntries ()
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Traits qualified as Traits

data TheStrangeCaseOfDrJekyllAndMrHydeDefs

instance IsHomebrewDefs TheStrangeCaseOfDrJekyllAndMrHydeDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
