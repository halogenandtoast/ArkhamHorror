{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosBag.RevealStrategy where

import Arkham.Prelude
import Data.Aeson.TH

data RevealStrategy
  = Reveal Int
  | RevealAndChoose Int Int
  | MultiReveal RevealStrategy RevealStrategy
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''RevealStrategy)
