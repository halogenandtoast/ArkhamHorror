{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosBag.RevealStrategy where

import Arkham.Prelude

import Data.Aeson.TH

data RevealStrategy
  = Reveal Int
  | RevealAndChoose Int Int
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

$(deriveJSON defaultOptions ''RevealStrategy)
