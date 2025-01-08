module Arkham.Campaigns.TheDunwichLegacy.Key where

import Arkham.Prelude

data TheDunwichLegacyKey
  = ProfessorWarrenRiceWasKidnapped
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
  | TheInvestigatorsRescuedDrHenryArmitage
  | TheInvestigatorsFailedToRecoverTheNecronomicon
  | TheInvestigatorsDestroyedTheNecronomicon
  | TheInvestigatorsTookCustodyOfTheNecronomicon
  | TheNecronomiconWasStolen
  | TheInvestigatorsWereDelayedOnTheirWayToDunwich
  | TheRitualWasCompleted
  | TheInvestigatorsPutSilasBishopOutOfHisMisery
  | TheInvestigatorsRestoredSilasBishop
  | TheInvestigatorsBanishedSilasBishop
  | SacrificedToYogSothoth
  | DrHenryArmitageSurvivedTheDunwichLegacy
  | ProfessorWarrenRiceSurvivedTheDunwichLegacy
  | DrFrancisMorganSurvivedTheDunwichLegacy
  | ZebulonWhateleySurvivedTheDunwichLegacy
  | EarlSawyerSurvivedTheDunwichLegacy
  | YouCalmedTheTownsfolk
  | YouWarnedTheTownsfolk
  | BroodEscapedIntoTheWild
  | NoBroodEscapedIntoTheWild
  | TheInvestigatorsEnteredTheGate
  | YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
  | TheInvestigatorsClosedTheTearInReality
  | YogSothothHasFledToAnotherDimension
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
