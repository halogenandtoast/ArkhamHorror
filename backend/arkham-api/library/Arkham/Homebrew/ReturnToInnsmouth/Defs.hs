{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ReturnToInnsmouth.Defs (module Arkham.Homebrew.ReturnToInnsmouth.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.ReturnToInnsmouth.CardDefEntries ()

data ReturnToInnsmouthDefs

{- | The set adds no traits and no actions of its own: "Deep One investigator" is
the official @Deep One@ trait granted by a card, so it needs no new door.
-}
instance IsHomebrewDefs ReturnToInnsmouthDefs where
  homebrewDefs = discoveredDefs $(generateHomebrewCardDefs)
