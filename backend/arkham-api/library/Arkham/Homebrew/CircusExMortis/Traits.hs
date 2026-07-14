{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by the Circus Ex Mortis campaign.

The list below is the single source of truth: 'declareHomebrewTraits' generates
a bidirectional pattern synonym for each name (so card code references a named,
typo-checked constructor exactly as it would a core trait) plus the aggregate
@traits@ list, which this campaign's @Defs.hs@ folds into the global trait
universe ('Arkham.Homebrew.DefsBase.hdTraits'). Compilation fails if any name
here already exists as a core trait — see 'declareHomebrewTraits'.

Note: @Moon@ is a location symbol here, not a trait, so it is intentionally
absent; card code gets it from the core location DSL.
-}
module Arkham.Homebrew.CircusExMortis.Traits (module Arkham.Homebrew.CircusExMortis.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "Camp"
  , "CircusTrain"
  , "Clearing"
  , "FreightCar"
  , "LiberPater"
  , "NewMoonCircus"
  , "Path"
  , "SpecialCar"
  , "Tainted"
  ]
