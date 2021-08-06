module Arkham.Types.ClassSymbol
  ( ClassSymbol(..)
  ) where

import Arkham.Prelude

data ClassSymbol
  = Guardian
  | Seeker
  | Survivor
  | Rogue
  | Mystic
  | Neutral
  deriving stock (Show, Eq, Generic, Bounded, Enum, Ord)
  deriving anyclass (ToJSON, FromJSON)
