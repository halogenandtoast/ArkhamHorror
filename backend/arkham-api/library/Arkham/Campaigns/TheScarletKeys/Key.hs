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
  | TheCellMeddledInAbarransAffairs
  | TheCellRefusedEcesOffer
  | TheLoversAreReunited
  | TheSanguineWatchersTormentContinues
  | Time
  | TuwileMasaiFledToBermuda
  | TuwileMasaiIsOnYourSide
  | WrongLeads
  | YouHaventSeenTheLastOfAlikiZoniUperetria
  | YouHaventSeenTheLastOfAmaranth
  | YouHaventSeenTheLastOfDesiderioDelgadoAlvarez
  | YouHaventSeenTheLastOfLaChicaRoja
  | YouHaventSeenTheLastOfTheBeastInACowlOfCrimson
  | YouHaventSeenTheLastOfTheClaretKnight
  | YouHaventSeenTheLastOfTheRedGlovedMan
  | YouHaventSeenTheLastOfTheSanguineWatcher
  | YouHaventSeenTheLastOfThorne
  | YouHaventSeenTheLastOfTzuSanNiang
  | YouKnowThePassphrase
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
