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
  | Between GameValue GameValue
  | AnyValue
  | GameValueOneOf [ValueMatcher]
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''ValueMatcher)
