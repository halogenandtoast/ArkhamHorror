{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.AndThenThereWereNone.Defs (module Arkham.Homebrew.AndThenThereWereNone.Defs) where

import Arkham.Homebrew.AndThenThereWereNone.CardDefEntries ()
import Arkham.Homebrew.AndThenThereWereNone.Traits qualified as Traits
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)

data AndThenThereWereNoneDefs

instance IsHomebrewDefs AndThenThereWereNoneDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
