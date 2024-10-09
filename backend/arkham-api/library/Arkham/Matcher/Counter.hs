{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Counter where

import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels

data CounterMatcher = HorrorCounter | DamageCounter | ClueCounter | DoomCounter | ResourceCounter
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "horror" CounterMatcher where
  fromLabel = HorrorCounter

instance IsLabel "damage" CounterMatcher where
  fromLabel = DamageCounter

instance IsLabel "clue" CounterMatcher where
  fromLabel = ClueCounter

$(deriveJSON defaultOptions ''CounterMatcher)
