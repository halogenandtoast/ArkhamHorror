{-# LANGUAGE TemplateHaskell #-}

module Arkham.Timing where

import Arkham.Prelude

import Data.Aeson.TH

data Timing = When | AtIf | After
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''Timing)
