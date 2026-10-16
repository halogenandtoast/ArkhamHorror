{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by And Then There Were None.

@Victim@ and @Culprit@ are the two faces of one card: ten [[Victim]] story
assets stand at the five outer locations, and act 1's advance flips exactly one
of them -- the one the icon tally convicts -- to its [[Culprit]] enemy side.

@Method@ is the murder weapon. Fifteen of them exist, ten make it into the game,
and their skill icons are the evidence the tally reads. @SoldierIsland@ is the
island itself; every location has it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Traits (module Arkham.Homebrew.AndThenThereWereNone.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "Culprit"
  , "Method"
  , "SoldierIsland"
  , "Victim"
  ]
