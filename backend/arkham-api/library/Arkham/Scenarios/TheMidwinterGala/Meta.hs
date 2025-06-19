module Arkham.Scenarios.TheMidwinterGala.Meta where

import Arkham.Prelude
import Arkham.Scenarios.TheMidwinterGala.Faction

data Meta = Meta {ally :: Faction, rival :: Faction}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

