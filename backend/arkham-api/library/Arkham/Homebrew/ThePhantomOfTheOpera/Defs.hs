{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.ThePhantomOfTheOpera.Defs (module Arkham.Homebrew.ThePhantomOfTheOpera.Defs) where

import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.Generate (generateHomebrewCardDefs)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefEntries ()
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.Kinds qualified as Kinds
import Arkham.Homebrew.ThePhantomOfTheOpera.Traits qualified as Traits

data ThePhantomOfTheOperaDefs

instance IsHomebrewDefs ThePhantomOfTheOperaDefs where
  homebrewDefs =
    (discoveredDefs $(generateHomebrewCardDefs))
      { hdTraits = Traits.traits
      , hdConcealedCardKinds =
          [ (Kinds.ThePhantom, Enemies.thePhantom)
          , (Kinds.BuriedSorrows, Enemies.buriedSorrows)
          , (Kinds.SimmeringAnger, Enemies.simmeringAnger)
          , (Kinds.StifledLaughter, Enemies.stifledLaughter)
          , (Kinds.ChaoticMinionA, Enemies.chaoticMinionA)
          , (Kinds.ChaoticMinionB, Enemies.chaoticMinionB)
          ]
      }
