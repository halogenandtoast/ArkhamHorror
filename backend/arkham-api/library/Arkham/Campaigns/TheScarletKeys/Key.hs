module Arkham.Campaigns.TheScarletKeys.Key where

import Arkham.Prelude

data TheScarletKeysKey
  = AmaranthHasLeftTheCoterie
  | DesiIsInYourDebt
  | EceDoesNotTrustTheCell
  | EceTrustsTheCell
  | ErasedFromExistence
  | LaChicaRojaIsOnYourSide
  | TheCellDidntDiscoverTheTruthInHavana
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
  | YouHaventSeenTheLastOfDesiderioDelgadoAlvarez
  | YouHaventSeenTheLastOfLaChicaRoja
  | YouHaventSeenTheLastOfTheClaretKnight
  | YouHaventSeenTheLastOfTheRedGlovedMan
  | YouHaventSeenTheLastOfTheSanguineWatcher
  | YouHaventSeenTheLastOfThorn
  | YouKnowThePassphrase
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
