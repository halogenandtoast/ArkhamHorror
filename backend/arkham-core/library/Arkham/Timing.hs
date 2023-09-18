{-# LANGUAGE TemplateHaskell #-}

module Arkham.Timing where

import Arkham.Prelude

import Data.Aeson.TH
import GHC.OverloadedLabels

data Timing = When | AtIf | After
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "when" Timing where
  fromLabel = When

instance IsLabel "at" Timing where
  fromLabel = AtIf

instance IsLabel "if" Timing where
  fromLabel = AtIf

instance IsLabel "after" Timing where
  fromLabel = After

$(deriveJSON defaultOptions ''Timing)
