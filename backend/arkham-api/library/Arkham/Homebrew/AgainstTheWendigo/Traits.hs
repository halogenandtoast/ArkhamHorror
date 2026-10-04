{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by Against the Wendigo.

The scenario divides its map into @Civilized@, @Wild@ and @Mystical@ ground and
hangs most of its rules off those; @Guide@ is the shared hook the Sarcee Guide
and Charlie Foxtail answer to, and @River@ (a core trait) is what the Navigate
and Walk Along the River actions move between.
-}
module Arkham.Homebrew.AgainstTheWendigo.Traits (module Arkham.Homebrew.AgainstTheWendigo.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "Animal"
  , "Civilized"
  , "Guide"
  , "Indigenous"
  , "Mystical"
  , "Sarcee"
  , "Wendigo"
  , "Wild"
  ]
