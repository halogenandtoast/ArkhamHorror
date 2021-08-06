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
  deriving stock (Ord, Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class HasKeywords a where
  toKeywords :: a -> Set Keyword
