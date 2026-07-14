{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by the Dark Matter campaign.

The list below is the single source of truth: 'declareHomebrewTraits' generates
a bidirectional pattern synonym for each name (so card code references a named,
typo-checked constructor exactly as it would a core trait) plus the aggregate
@traits@ list, which this campaign's @Defs.hs@ folds into the global trait
universe ('Arkham.Homebrew.DefsBase.hdTraits'). Compilation fails if any name
here already exists as a core trait — see 'declareHomebrewTraits'.
-}
module Arkham.Homebrew.DarkMatter.Traits (module Arkham.Homebrew.DarkMatter.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "AI"
  , "Access"
  , "Alien"
  , "AsteroidBelt"
  , "Brain"
  , "Carcosa"
  , "Colony"
  , "Data"
  , "Device"
  , "Earth"
  , "Elbrus"
  , "Interface"
  , "Liminal"
  , "Machine"
  , "Mars"
  , "Medical"
  , "Memory"
  , "Moon"
  , "Nightmare"
  , "NostalgiaII"
  , "Pluto"
  , "Quantum"
  , "School"
  , "Simulation"
  , "Starship"
  , "Tatterdemalion"
  , "Virtual"
  ]
