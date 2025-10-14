module Arkham.Campaigns.TheScarletKeys.Key where

import Arkham.Prelude

data TheScarletKeysKey
  = Time
  | YouHaventSeenTheLastOfTheRedGlovedMan
  | TheCellKnowsOfDesisPast
  | TheCellKnowsAmaranthsRealName
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
