{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.AgainstTheWendigo.Defs (module Arkham.Homebrew.AgainstTheWendigo.Defs) where

import Arkham.Homebrew.AgainstTheWendigo.Actions qualified as Actions
import Arkham.Homebrew.AgainstTheWendigo.CardDefEntries ()
import Arkham.Homebrew.AgainstTheWendigo.Traits qualified as Traits
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)

data AgainstTheWendigoDefs

instance IsHomebrewDefs AgainstTheWendigoDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs))
      { hdTraits = Traits.traits
      , hdActions = Actions.actions
      , hdActionAffordability = Actions.actionAffordability
      }
