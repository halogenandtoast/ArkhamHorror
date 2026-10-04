{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Defs (module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefEntries ()

data ReturnToTheInnsmouthConspiracyDefs

{- | The set adds no traits and no actions of its own: "Deep One investigator" is
the official @Deep One@ trait granted by a card, so it needs no new door.
-}
instance IsHomebrewDefs ReturnToTheInnsmouthConspiracyDefs where
  homebrewDefs = discoveredDefs $(generateHomebrewCardDefs)
