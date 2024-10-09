{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Aspect where

import Arkham.Aspect.Types
import Arkham.Prelude
import Data.Aeson.TH

data AspectMatcher = AspectIs Aspect
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''AspectMatcher)
