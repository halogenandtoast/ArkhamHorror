module Arkham.Types.ClassSymbol
  ( ClassSymbol(..)
  )
where

import Arkham.Json
import ClassyPrelude

data ClassSymbol
  = Guardian
  | Seeker
  | Survivor
  | Rogue
  | Mystic
  | Neutral
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

