{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.DarkMatter.Defs (module Arkham.Homebrew.DarkMatter.Defs) where

import Arkham.Homebrew.DarkMatter.Actions qualified as Actions
import Arkham.Homebrew.DarkMatter.CardDefEntries ()
import Arkham.Homebrew.DarkMatter.Traits qualified as Traits
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)

data DarkMatterDefs

{- | Card definitions are discovered: every @<name> :: CardDef@ under
@CardDefs/@ is registered, and sorted by its card type (see 'discoveredDefs').
Defs printed on a player card back declare @<name> :: PlayerCardDef@ instead.
-}
instance IsHomebrewDefs DarkMatterDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs))
      { hdTraits = Traits.traits
      , hdActions = Actions.actions
      , hdActionAffordability = Actions.actionAffordability
      }
