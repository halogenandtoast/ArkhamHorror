{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.History where

import Arkham.Matcher.Value
import Arkham.Prelude
import Data.Aeson.TH

data HistoryMatcher = DefeatedEnemiesWithTotalHealth ValueMatcher
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''HistoryMatcher)
