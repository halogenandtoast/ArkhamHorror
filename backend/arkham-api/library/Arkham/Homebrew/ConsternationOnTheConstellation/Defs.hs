{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ConsternationOnTheConstellation.Defs (
  module Arkham.Homebrew.ConsternationOnTheConstellation.Defs,
) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefEntries ()
import Arkham.Homebrew.ConsternationOnTheConstellation.Traits qualified as Traits
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)

data ConsternationOnTheConstellationDefs

instance IsHomebrewDefs ConsternationOnTheConstellationDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
