module Arkham.Campaigns.TheForgottenAge.Key where

import Arkham.Prelude

data TheForgottenAgeKey
  = TheInvestigatorsWereForcedToWaitForAdditionalSupplies
  | IchtacaObservedYourProgressWithKeenInterest
  | AlejandroFollowedTheInvestigatorsIntoTheRuins
  | IchtacaIsWaryOfTheInvestigators
  | TheInvestigatorsHaveEarnedIchtacasTrust
  | AlejandroChoseToRemainAtCamp
  | TheInvestigatorsClearedAPathToTheEztliRuins
  | YigsFury
  | TheInvestigatorsRecoveredTheRelicOfAges
  | AlejandroRecoveredTheRelicOfAges
  | TheHarbingerIsStillAlive
  | TheInvestigatorsGaveCustodyOfTheRelicToAlejandro
  | TheInvestigatorsHaveEarnedAlejandrosTrust
  | TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
  | AlejandroIsContinuingHisResearchOnHisOwn
  | YouAreForgingYourOwnWay
  | TheInvestigatorsFoundTheMissingRelic
  | TheRelicIsMissing
  | TheInvestigatorsRescuedAlejandro
  | AlejandroIsMissing
  | TheInvestigatorsForgedABondWithIchtaca
  | IchtacaIsInTheDark
  | PathsAreKnownToYou
  | IchtacaHasConfidenceInYou
  | TheInvestigatorsMappedOutTheWayForward
  | IchtacasFaithIsRestored
  | TheJungleWatches
  | TheInvestigatorsCooperatedWithTheYithians
  | TheInvestigatorsResistedCaptivity
  | TheInvestigatorsHadTheirMemoriesExpunged
  | TheProcessWasPerfected
  | TheProcessWasSuccessful
  | TheProcessBackfired
  | TheProcessBackfiredSpectacularly
  | AlejandroRemembersEverything
  | AlejandroIsSetAgainstYou
  | IchtacaIsSetAgainstYou
  | TheInvestigatorsFellIntoTheDepths
  | TheNexusIsNear
  | TheBraziersAreLit
  | TheBraziersRemainUnlit
  | TheInvestigatorsMendedTheTearInTheFabricOfTime
  | TheInvestigatorsSavedTheCivilizationOfTheSerpents
  | TheInvestigatorsSavedTheCivilizationOfTheYithians
  | TheFabricOfTimeIsUnwoven
  | TheInvestigatorsTurnedBackTime
  | TheInvestigatorsSealedTheRelicOfAgesForever
  | TheInvestigatorsRecruitedTheHelpOfAnotherExpedition
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
