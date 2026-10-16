{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TheInvisibleMan.Defs (module Arkham.Homebrew.TheInvisibleMan.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.TheInvisibleMan.CardDefEntries ()
import Arkham.Homebrew.TheInvisibleMan.Traits qualified as Traits

data TheInvisibleManDefs

instance IsHomebrewDefs TheInvisibleManDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
