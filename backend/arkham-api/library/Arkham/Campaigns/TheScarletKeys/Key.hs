module Arkham.Campaigns.TheScarletKeys.Key where

import Arkham.Prelude

data TheScarletKeysKey
  = AmaranthHasLeftTheCoterie
  | EceDoesNotTrustTheCell
  | EceTrustsTheCell
  | ErasedFromExistence
  | LaChicaRojaIsOnYourSide
  | TheCellHasBeenAmbushed
  | TheCellIsDecievingEce
  | TheCellIsWorkingWithEce
  | TheCellKnowsAmaranthsRealName
  | TheCellKnowsOfDesisPast
  | TheCellMadeADealWithDesi
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
