{-# LANGUAGE TemplateHaskell #-}

{- | The concealed mini-cards this scenario adds. @Decoy@ is reused from core --
the scenario requires the decoy mini-cards from The Scarlet Keys.
-}
module Arkham.Homebrew.ThePhantomOfTheOpera.Kinds (module Arkham.Homebrew.ThePhantomOfTheOpera.Kinds) where

import Arkham.Homebrew.TH (declareHomebrewConcealedCardKinds)

declareHomebrewConcealedCardKinds
  [ "BuriedSorrows"
  , "ChaoticMinionA"
  , "ChaoticMinionB"
  , "SimmeringAnger"
  , "StifledLaughter"
  , "ThePhantom"
  ]
