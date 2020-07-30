module Arkham.Types.Keyword where

import ClassyPrelude
import Data.Aeson

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
