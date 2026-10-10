{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by The Masque of the Red Death.

@Victim@ is the flip side of the masquerade: each [[Guest]] story asset starts
masked and parleyable, and act 1's advance flips every one of them to its
@Victim@ face, which has health, sanity and a @Forced@ ability that pins you to
its location. The Red Death attacks each @Victim@ story asset at its location,
so the trait is also what tells it which cards it kills.

@Disease@ carries one treachery (Sudden Symptoms). Neither trait exists in core.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.Traits (module Arkham.Homebrew.TheMasqueOfTheRedDeath.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "Disease"
  , "Victim"
  ]
