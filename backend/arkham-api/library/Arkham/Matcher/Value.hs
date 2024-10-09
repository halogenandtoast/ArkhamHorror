{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Value where

import Arkham.GameValue
import Arkham.Prelude
import Data.Aeson.TH

data ValueMatcher
  = LessThan GameValue
  | GreaterThan GameValue
  | LessThanOrEqualTo GameValue
  | GreaterThanOrEqualTo GameValue
  | EqualTo GameValue
  | AnyValue
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''ValueMatcher)
