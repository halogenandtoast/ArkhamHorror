{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.History where

import Arkham.Id
import Arkham.Matcher.Value
import Arkham.Prelude
import Arkham.Trait (Trait)
import Data.Aeson.TH

data HistoryMatcher
  = DefeatedEnemiesWithTotalHealth ValueMatcher
  | DefeatedEnemyWithTraitAt Trait LocationId
  | AttackedByAnyEnemies
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''HistoryMatcher)
