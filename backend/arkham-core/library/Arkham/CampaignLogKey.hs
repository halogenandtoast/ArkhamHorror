module Arkham.CampaignLogKey where

import Arkham.Prelude hiding (toLower)
import Arkham.Classes.GameLogger
import Arkham.Card.CardCode
import Arkham.Campaigns.TheCircleUndone.Memento
import Control.Monad.Fail
import Data.Char (isUpper, toLower)
import Data.Typeable

data Recorded a = Recorded a | CrossedOut a
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

recordedCardCodes :: [SomeRecorded] -> [CardCode]
recordedCardCodes [] = []
recordedCardCodes (SomeRecorded RecordableCardCode (Recorded a) : as) = a : recordedCardCodes as
recordedCardCodes (_ : as) = recordedCardCodes as

unrecorded :: forall a. Recordable a => SomeRecorded -> Maybe a
unrecorded (SomeRecorded _ (rec :: Recorded b)) = case eqT @a @b of
  Just Refl -> case rec of
    Recorded a -> Just a
    CrossedOut a -> Just a
  Nothing -> Nothing

data CampaignLogKey
  = DrivenInsaneInvestigators
  | KilledInvestigators
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
  -- ^ The Forgotten Age
  | MissingPersons
  | WasTakenByTheWatcher
  | WasClaimedBySpecters
  | DisappearedIntoTheMist
  | WasPulledIntoTheSpectralRealm
  | PiecesOfEvidenceWereLeftBehind
  | YouAreBeingHunted
  | YouHaveAcceptedYourFate
  | YouHaveRejectedYourFate
  | TheWitches'SpellWasBroken
  | TheWitches'SpellWasCast
  | MementosDiscovered
  | TheInvestigatorsAreOnGavriella'sTrail
  | TheInvestigatorsAreOnJerome'sTrail
  | TheInvestigatorsAreOnPenny'sTrail
  | TheInvestigatorsAreOnValentino'sTrail
  | TheInvestigatorsEscapedTheSpectralRealm
  | TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
  | TheInvestigatorsAreNeverSeenOrHeardFromAgain
  | JosefDisappearedIntoTheMist
  | TheInvestigatorsAreEnemiesOfTheLodge
  | TheInvestigatorsRescuedJosef
  | JosefIsAliveAndWell
  | TheInvestigatorsAreMembersOfTheLodge
  | TheInvestigatorsAreDeceivingTheLodge
  | TheInvestigatorsToldTheLodgeAboutTheCoven
  | TheInvestigatorsHidTheirKnowledgeOfTheCoven
  -- ^ The Circle Undone
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
  | YouHaveTranslatedTheTome
  -- ^ Player Cards
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)

instance ToGameLoggerFormat CampaignLogKey where
  format = pack . go . show
    where
      go :: String -> String
      go [] = []
      go (x:xs) = toLower x : go' xs

      go' :: String -> String
      go' [] = []
      go' (x:xs) | isUpper x = ' ' : toLower x : go' xs
      go' (x:xs) = x : go' xs

class (ToJSON a, FromJSON a, Eq a, Show a, Typeable a) => Recordable a where
  recordableType :: RecordableType a

instance Recordable CardCode where
  recordableType = RecordableCardCode

instance Recordable Memento where
  recordableType = RecordableMemento

recorded :: forall a. Recordable a => a -> SomeRecorded
recorded a = SomeRecorded (recordableType @a) (Recorded a)

crossedOut :: forall a. Recordable a => a -> SomeRecorded
crossedOut a = SomeRecorded (recordableType @a) (CrossedOut a)

data RecordableType a where
  RecordableCardCode :: RecordableType CardCode
  RecordableMemento :: RecordableType Memento

data SomeRecordableType where
  SomeRecordableType :: RecordableType a -> SomeRecordableType

deriving stock instance Show (RecordableType a)
deriving stock instance Eq (RecordableType a)

instance ToJSON (RecordableType a) where
  toJSON = toJSON . show

instance FromJSON SomeRecordableType where
  parseJSON = withText "RecordableType" $ \case
    "RecordableCardCode" -> pure $ SomeRecordableType RecordableCardCode
    "RecordableMemento" -> pure $ SomeRecordableType RecordableMemento
    other -> fail $ "No such recordable type: " <> unpack other

data SomeRecorded where
  SomeRecorded :: Recordable a => RecordableType a -> Recorded a -> SomeRecorded

deriving stock instance Show SomeRecorded

instance Eq SomeRecorded where
  (SomeRecorded _ (a :: a)) == (SomeRecorded _ (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON SomeRecorded where
  toJSON (SomeRecorded rType rVal) = object ["recordType" .= rType, "recordVal" .= rVal]

instance FromJSON SomeRecorded where
  parseJSON = withObject "SomeRecorded" $ \o -> do
    rType <- o .: "recordType"
    case rType of
      SomeRecordableType RecordableCardCode -> do
        rVal <- o .: "recordVal"
        pure $ SomeRecorded RecordableCardCode rVal
      SomeRecordableType RecordableMemento -> do
        rVal <- o .: "recordVal"
        pure $ SomeRecorded RecordableMemento rVal

