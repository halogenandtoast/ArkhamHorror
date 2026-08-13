{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.CircusExMortis.Defs (module Arkham.Homebrew.CircusExMortis.Defs) where

import Arkham.Homebrew.CircusExMortis.CardDefEntries ()
import Arkham.Homebrew.CircusExMortis.Traits qualified as Traits
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)

data CircusExMortisDefs

{- | Card definitions are discovered: every @<name> :: CardDef@ under
@CardDefs/@ is registered, and sorted by its card type (see 'discoveredDefs').
Defs printed on a player card back declare @<name> :: PlayerCardDef@ instead.
-}
instance IsHomebrewDefs CircusExMortisDefs where
  homebrewDefs = (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
