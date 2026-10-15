{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Defs (module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefEntries ()
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Traits qualified as Traits

data TheFallOfTheHouseOfUsherDefs

instance IsHomebrewDefs TheFallOfTheHouseOfUsherDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
