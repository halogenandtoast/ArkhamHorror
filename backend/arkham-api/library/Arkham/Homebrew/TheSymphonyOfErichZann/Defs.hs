{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheSymphonyOfErichZann.Defs (module Arkham.Homebrew.TheSymphonyOfErichZann.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefEntries ()
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as Traits

data TheSymphonyOfErichZannDefs

instance IsHomebrewDefs TheSymphonyOfErichZannDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
