module Arkham.Campaigns.TheScarletKeys.Key where

import Arkham.Prelude

data TheScarletKeysKey
  = Time
  | YouHaventSeenTheLastOfTheRedGlovedMan
  | TheCellKnowsOfDesisPast
  | TheCellKnowsAmaranthsRealName
  | YouHaventSeenTheLastOfAmaranth
  | TheLoversAreReunited
  | AmaranthHasLeftTheCoterie
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
