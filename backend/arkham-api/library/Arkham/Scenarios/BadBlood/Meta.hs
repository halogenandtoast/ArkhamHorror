module Arkham.Scenarios.BadBlood.Meta where

import Arkham.Prelude

data Meta = Meta {agnesMemories :: Int, elspethMemories :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyMeta :: Meta
emptyMeta = Meta 0 0
