module Arkham.Types.ClassSymbol
  ( ClassSymbol(..)
  )
where

import ClassyPrelude

import Arkham.Json

data ClassSymbol
  = Guardian
  | Seeker
  | Survivor
  | Rogue
  | Mystic
  | Neutral
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
