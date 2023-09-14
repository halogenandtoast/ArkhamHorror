module Arkham.ClassSymbol (
  ClassSymbol (..),
) where

import Arkham.Prelude

data ClassSymbol
  = Guardian
  | Seeker
  | Survivor
  | Rogue
  | Mystic
  | Neutral
  | Mythos
  deriving stock (Show, Eq, Generic, Bounded, Enum, Ord, Data)
  deriving anyclass (ToJSON, FromJSON, Hashable)
