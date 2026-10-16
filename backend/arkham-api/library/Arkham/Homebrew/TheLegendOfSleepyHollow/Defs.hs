{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheLegendOfSleepyHollow.Defs (module Arkham.Homebrew.TheLegendOfSleepyHollow.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefEntries ()
import Arkham.Homebrew.TheLegendOfSleepyHollow.Traits qualified as Traits

data TheLegendOfSleepyHollowDefs

instance IsHomebrewDefs TheLegendOfSleepyHollowDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
