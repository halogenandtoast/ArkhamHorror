{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosBag.RevealStrategy where

import Arkham.Prelude
import Data.Aeson.TH

data RevealStrategy
  = Reveal Int
  | RevealAndChoose Int Int
  | MultiReveal RevealStrategy RevealStrategy
  deriving stock (Show, Eq, Ord, Data)

{- | What becomes of the extra tokens a @DrawAdditionalChaosTokens@ reveals:
"reveal and resolve two additional chaos tokens" is 'ResolveEach', while
"reveal one additional token and choose one to resolve" is 'ResolveOne'.
-}
data AdditionalReveals = ResolveEach | ResolveOne
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''RevealStrategy)
$(deriveJSON defaultOptions ''AdditionalReveals)
