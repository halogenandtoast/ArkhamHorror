{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Key where

import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels

data KeyMatcher = AnyKey
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" KeyMatcher where
  fromLabel = AnyKey

$(deriveJSON defaultOptions ''KeyMatcher)
