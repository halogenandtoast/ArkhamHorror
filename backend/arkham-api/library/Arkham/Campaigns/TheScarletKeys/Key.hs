module Arkham.Campaigns.TheScarletKeys.Key where

import Arkham.Prelude

data TheScarletKeysKey
  = AmaranthHasLeftTheCoterie
  | LaChicaRojaIsOnYourSide
  | TheCellKnowsAmaranthsRealName
  | TheCellKnowsOfDesisPast
  | TheLoversAreReunited
  | TheSanguineWatchersTormentContinues
  | Time
  | YouHaventSeenTheLastOfAmaranth
  | YouHaventSeenTheLastOfLaChicaRoja
  | YouHaventSeenTheLastOfTheRedGlovedMan
  | YouHaventSeenTheLastOfTheSanguineWatcher
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
