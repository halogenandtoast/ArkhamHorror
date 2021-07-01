module Arkham.Types.Keyword where

import Arkham.Prelude

data Keyword
  = Alert
  | Aloof
  | Fast
  | Hunter
  | Massive
  | Peril
  | Retaliate
  | Surge
  | Uses Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

class HasKeywords a where
  toKeywords :: a -> HashSet Keyword
