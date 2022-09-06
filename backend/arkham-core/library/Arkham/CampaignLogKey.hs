module Arkham.CampaignLogKey where

import Arkham.Prelude

data Recorded a = Recorded a | CrossedOut a
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

unrecorded :: Recorded a -> a
unrecorded = \case
  Recorded a -> a
  CrossedOut a -> a

data CampaignLogKey
  = DrivenInsaneInvestigators
  | GhoulPriestIsStillAlive
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
  -- ^ The Night of the Zealot
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
  -- ^ The Dunwich Legacy
  | TheStrangerIsOnToYou
  | ChasingTheStranger
  | YouTriedToWarnThePolice
  | ThePoliceAreSuspiciousOfYou
  | YouChoseNotToGoToThePolice
  | Doubt
  | Conviction
  | VIPsInterviewed
  | VIPsSlain
  | YouIntrudedOnASecretMeeting
  | YouFledTheDinnerParty
  | YouSlayedTheMonstersAtTheDinnerParty
  | YouTookTheOnyxClasp
  | YouLeftTheOnyxClaspBehind
  | YouDestroyedTheOathspeaker
  | TheFollowersOfTheSignHaveFoundTheWayForward
  | TheKingClaimedItsVictims
  | TheInvestigatorsWereAttackedAsTheyEscapedTheAsylum
  | TheInvestigatorsEscapedTheAsylum
  | YouIgnoredDanielsWarning
  | YouHeadedDanielsWarning
  | YouDidNotEscapeTheGazeOfThePhantom
  | YouFoundNigelsHome
  | YouFoundNigelEngram
  | YouWereUnableToFindNigel
  | YouAwokeInsideTheCatacombs
  | YouEnteredTheCatacombsOnYourOwn
  | YouKnowTheSiteOfTheGate
  | ReadActII
  | YouOpenedThePathBelow
  | YouOpenedThePathAbove
  | TheRealmOfCarcosaMergedWithOurOwnAndHasturRulesOverThemBoth
  | TheInvestigatorsPreventedHasturFromEscapingHisPrison
  | HasturHasYouInHisGrasp
  | Possessed
  -- ^ The Path to Carcosa
  | TheInvestigatorsWereForcedToWaitForAdditionalSupplies
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
  -- ^ The Forgotten Age
  | TheRougarouContinuesToHauntTheBayou
  | TheRougarouIsDestroyed
  | TheRougarouEscapedAndYouEmbracedTheCurse
  -- ^ Curse of the Rougarou
  | ManyWereSacrificedToCnidathquaDuringTheCarnivale
  | TheSunBanishedCnidathquaIntoTheDepths
  | CnidathquaRetreatedToNurseItsWounds
  -- ^ Carnevale of Horrors
  | YouHaveIdentifiedTheSolution
  | YouHaveTranslatedTheGlyphs
  | YouHaveIdentifiedTheStone
  | DoomApproaches
  | TheHourIsNigh
  -- ^ Player Cards
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)
