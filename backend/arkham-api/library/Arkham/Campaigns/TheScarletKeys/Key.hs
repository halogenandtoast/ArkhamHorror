module Arkham.Campaigns.TheScarletKeys.Key where

import Arkham.Prelude

data TheScarletKeysKey
  = AmaranthHasLeftTheCoterie
  | EceDoesNotTrustTheCell
  | EceTrustsTheCell
  | LaChicaRojaIsOnYourSide
  | TheCellIsDecievingEce
  | TheCellIsWorkingWithEce
  | TheCellKnowsAmaranthsRealName
  | TheCellKnowsOfDesisPast
  | TheCellRefusedEcesOffer
  | TheLoversAreReunited
  | TheSanguineWatchersTormentContinues
  | Time
  | TuwileMasaiFledToBermuda
  | TuwileMasaiIsOnYourSide
  | YouHaventSeenTheLastOfAlikiZoniUperetria
  | YouHaventSeenTheLastOfAmaranth
  | YouHaventSeenTheLastOfDesi
  | YouHaventSeenTheLastOfLaChicaRoja
  | YouHaventSeenTheLastOfTheClaretKnight
  | YouHaventSeenTheLastOfTheRedGlovedMan
  | YouHaventSeenTheLastOfTheSanguineWatcher
  | YouHaventSeenTheLastOfThorn
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
