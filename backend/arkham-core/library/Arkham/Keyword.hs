module Arkham.Keyword where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Matcher

data Keyword
  = Alert
  | Aloof
  | Fast
  | Hidden
  | Hunter
  | Massive
  | Peril
  | Retaliate
  | Surge
  | Uses Int
  | Exceptional
  | Permanent
  | Seal TokenMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

class HasKeywords a where
  toKeywords :: a -> HashSet Keyword
