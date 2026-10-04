{-# LANGUAGE TemplateHaskell #-}

{- | Trait values owned by Consternation on the Constellation.

@Deck1@, @Deck2@ and @Deck3@ are the three decks of the SS Constellation. They
are load-bearing rather than flavor: Taking on Water and agenda 3b both exhaust
"a ready location with the lowest possible @Deck@ number", act 2b exhausts every
@Deck 1@ location, and resolution 3 asks whether every @Deck 1@ and @Deck 2@
location is exhausted.

@Retired@ is Inspector Legrasse's.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.Traits (
  module Arkham.Homebrew.ConsternationOnTheConstellation.Traits,
) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits
  [ "Deck1"
  , "Deck2"
  , "Deck3"
  , "Retired"
  ]
