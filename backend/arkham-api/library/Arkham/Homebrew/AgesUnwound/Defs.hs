{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.AgesUnwound.Defs (module Arkham.Homebrew.AgesUnwound.Defs) where

import Arkham.Homebrew.AgesUnwound.CardDefEntries ()
import Arkham.Homebrew.AgesUnwound.Traits qualified as Traits
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)

data AgesUnwoundDefs

{- | Card definitions are discovered: every @<name> :: CardDef@ under
@CardDefs/@ is registered, and sorted by its card type (see 'discoveredDefs').
Defs printed on a player card back declare @<name> :: PlayerCardDef@ instead.
-}
instance IsHomebrewDefs AgesUnwoundDefs where
  homebrewDefs = (discoveredDefs $(generateHomebrewCardDefs)) {hdTraits = Traits.traits}
