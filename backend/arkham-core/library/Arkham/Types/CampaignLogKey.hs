module Arkham.Types.CampaignLogKey where

import Arkham.Prelude

data CampaignLogKey
  = GhoulPriestIsStillAlive
  | YourHouseIsStillStanding
  | YourHouseHasBurnedToTheGround
  | LitaWasForcedToFindOthersToHelpHerCause
  | CultistsWeInterrogated
  | CultistsWhoGotAway
  | ItIsPastMidnight
  | ArkhamSuccumbedToUmordhothsTerribleVengeance
  | TheRitualToSummonUmordhothWasBroken
  | TheInvestigatorsRepelledUmordoth
  | TheInvestigatorsSacrificedLitaChantlerToUmordhoth
  | ProfessorWarrenRiceWasKidnapped
  | TheInvestigatorsRescuedProfessorWarrenRice
  | TheInvestigatorsFailedToSaveTheStudents
  | TheStudentsWereRescued
  | TheExperimentWasDefeated
  | InvestigatorsWereUnconsciousForSeveralHours
  | OBannionGangHasABoneToPickWithTheInvestigators
  | DrFrancisMorganWasKidnapped
  | TheInvestigatorsRescuedDrFrancisMorgan
  | NaomiHasTheInvestigatorsBacks
  | DrHenryArmitageWasKidnapped
  | TheInvestigatorsFailedToRecoverTheNecronomicon
  | TheInvestigatorsDestroyedTheNecronomicon
  | TheInvestigatorsTookCustodyOfTheNecronomicon
  | TheNecronomiconWasStolen
  | TheInvestigatorsWereDelayedOnTheirWayToDunwich
  | TheRitualWasCompleted
  | TheInvestigatorsPutSilasBishopOutOfHisMisery
  | TheInvestigatorsRestoredSilasBishop
  | TheInvestigatorsBanishedSilasBishop
  | TheRougarouContinuesToHauntTheBayou
  | TheRougarouIsDestroyed
  | TheRougarouEscapedAndYouEmbracedTheCurse
  | YouHaveIdentifiedTheSolution
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)
