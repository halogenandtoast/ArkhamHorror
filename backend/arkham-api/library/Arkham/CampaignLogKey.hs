{-# LANGUAGE TemplateHaskell #-}

module Arkham.CampaignLogKey where

import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card.CardCode
import Arkham.Classes.GameLogger
import Arkham.Prelude hiding (toLower)
import Control.Monad.Fail
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Char (isUpper, toLower)
import Data.Data
import Data.Text qualified as T

data CampaignLogKey
  = -- | The Night of the Zealot
    DrivenInsaneInvestigators
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
  | -- | The Dunwich Legacy
    ProfessorWarrenRiceWasKidnapped
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
  | -- | The Path to Carcosa
    TheStrangerIsOnToYou
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
  | -- | The Forgotten Age
    TheInvestigatorsWereForcedToWaitForAdditionalSupplies
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
  | -- | The Circle Undone
    MissingPersons
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
  | TheInvestigatorsSurvivedTheWatchersEmbrace
  | HereticsWereUnleashedUntoArkham
  | TheGuardianOfTheTrapEmerged
  | TheInvestigatorsDiscoveredHowToOpenThePuzzleBox
  | TheGuardianOfTheTrapEmergedAndWasDefeated
  | TheInvestigatorsKeptsTheirMementosHidden
  | TheInvestigatorsWereInductedIntoTheInnerCircle
  | TheInvestigatorsSidedWithTheLodge
  | TheInvestigatorsSidedWithTheCoven
  | GavriellaIsAlive
  | JeromeIsAlive
  | PennyIsAlive
  | ValentinoIsAlive
  | TheTrueWorkOfTheSilverTwilightLodgeHasBegun
  | CarlSanfordPossessesTheSecretsOfTheUniverse
  | AnetteMasonIsPossessedByEvil
  | GavriellaIsDead
  | JeromeIsDead
  | PennyIsDead
  | ValentinoIsDead
  | TheInvestigatorsContinuedAlone
  | TheInvestigatorsAskedAnetteForAssistance
  | TheInvestigatorsArrestedAnette
  | AnetteTaughtYouTheSpellsOfOld
  | TheInvestigatorsAskedSanfordForAssistance
  | TheInvestigatorsArrestedSanford
  | TheInvestigatorsAssumedControlOfTheSilverTwilightLodge
  | DoomDrawsEverCloser
  | ThePathWindsBeforeYou
  | AzathothDevouredTheUniverse
  | TheLeadInvestigatorHasJoinedThePipersOfAzathoth
  | AzathothSlumbersForNow
  | TheInvestigatorsSignedTheBlackBookOfAzathoth
  | -- | The Dream-Eaters
    TheDreamersStrayedFromThePath
  | TheInvestigatorsWereSavedByRandolphCarder
  | TheCatsCollectedTheirTributeFromTheZoogs
  | TheInvestigatorsParleyedWithTheZoogs
  | DrMaheswaranJoinedTheInvestigation
  | DrMaheswaranStayedWithHerPatients
  | StepsOfTheBridge
  | DrMaheswaran'sFateIsUnknown
  | RandolphEscapedTheHospitalOnHisOwn
  | RandolphEscapedTheHospitalWithTheInvestigators
  | DrMaheswaranIsAlive
  | DrMaheswaranIsMissing
  | TheBlackCatDeliveredNewsOfYourPlight
  | TheBlackCatSharedKnowledgeOfTheDreamlands
  | TheBlackCatWarnedTheOthers
  | OkayFineHaveItYourWayThen
  | TheDreamersGrowWeaker
  | YouAreOnYourOwn
  | TheBlackCatHasAHunch
  | TheBlackCatIsAtYourSide
  | YouAskedForIt
  | TheInvestigatorsForcedTheirWayIntoTheTemple
  | EvidenceOfKadath
  | HasBrokenTheLawOfUlthar
  | WasCaptured
  | VirgilWasCaptured
  | RandolphEludedCapture
  | RandolphWasCaptured
  | RandolphSurvivedTheDescent
  | RandolphDidNotSurviveTheDescent
  | TheInvestigatorsPossessTheSilverKey
  | TheBlackCatRequestedAidFromTheOthers
  | TheBlackCatSharedKnowledgeOfTheUnderworld
  | TheBlackCatIsSearchingForTheTruth
  | TheDreamersKnowOfAnotherPath
  | TheInvestigatorsWereCarriedToTheColdWastes
  | TheInvestigatorsTraveledToTheColdWastes
  | RandolphCarterDidNotSurviveTheVoyage
  | RandolphSurvivedTheVoyage
  | TheInvestigatorsFoundAWayOutOfTheUnderworld
  | TheBlackCatSpokeOfNyarlathotep
  | TheBlackCatSpokeOfAtlachNacha
  | TheBlackCatKnowsTheTruth
  | Nyarlathotep'sInvasionHasBegun
  | TheDreamersEscapedFromNyarlathotep'sGrasp
  | TheDreamersBanishedNyarlathotep
  | TheDreamersAwoke
  | TheDreamersStayedInTheDreamlandsForever
  | TheDreamersTraveledBeneathTheMonastery
  | TheInvestigatorsAreTrappedInAtlachNacha'sRealm
  | TheBridgeWasCompleted
  | TheBridgeWasDestroyed
  | TheInvestigatorsReturnedToReality
  | TheInvestigatorsNeverEscaped
  | TheInvestigatorsAreStillInTheDreamlands
  | -- | The Innsmouth Conspiracy
    MemoriesRecovered
  | OutForBlood
  | TheMissionFailed
  | TheMissionWasSuccessful
  | PossibleSuspects
  | PossibleHideouts
  | InnsmouthWasConsumedByTheRisingTide
  | TheInvestigatorsMadeItSafelyToTheirVehicles
  | TheTideHasGrownStronger
  | TheTerrorOfDevilReefIsStillAlive
  | TheTerrorOfDevilReefIsDead
  | TheIdolWasBroughtToTheLighthouse
  | TheMantleWasBroughtToTheLighthouse
  | TheHeaddressWasBroughtToTheLighthouse
  | TheInvestigatorsReachedFalconPointBeforeSunrise
  | TheInvestigatorsReachedFalconPointAfterSunrise
  | PossessesADivingSuit
  | TheInvestigatorsPossessAMapOfYhaNthlei
  | TheInvestigatorsPossessTheKeyToYhaNthlei
  | DagonHasAwakened
  | DagonStillSlumbers
  | TheOrdersRitualWasDisrupted
  | TheGatekeeperHasBeenDefeated
  | TheGuardianOfYhanthleiIsDispatched
  | TheGatewayToYhanthleiRecognizesYouAsTheRightfulKeeper
  | TheInvestigatorsEscapedYhanthlei
  | ThePlotOfTheDeepOnesWasThwarted
  | TheFloodHasBegun
  | AgentHarpersMissionIsComplete
  | TheRichesOfTheDeepAreLostForever
  | AgentHarpersMissionIsCompleteButAtWhatCost
  | TheRichesOfTheDeepAreLostForeverButAtWhatCost
  | TheDeepOnesHaveFloodedTheEarth
  | TheHorribleTruth
  | -- | Edge of the Earth
    TheInvestigatorsConvincedDyerToAllowTheExpedition
  | TheInvestigatorsDidNotBelieveDyersReport
  | WasKilledInThePlaneCrash
  | SuppliesRecovered
  | Camp_CrashSite
  | Camp_FrozenShores
  | Camp_TreacherousPath
  | Camp_PrecariousIceSheet
  | Camp_BroadSnowdrifts
  | Camp_IcyWastes
  | Camp_RockyCrags
  | Camp_SnowGraves
  | Camp_IcebreakerLanding
  | Camp_FrigidCave
  | Camp_BarrierCamp
  | Camp_RemnantsOfLakesCamp
  | Camp_CrystallineCavern
  | TheTeamBarelyEscapedTheIceShelf
  | TheTeamDefeatedTheHuntingCreatures
  | TheTeamFledToTheMountains
  | LocationsRevealed
  | -- | Curse of the Rougarou
    TheRougarouContinuesToHauntTheBayou
  | TheRougarouIsDestroyed
  | TheRougarouEscapedAndYouEmbracedTheCurse
  | -- | Carnevale of Horrors
    ManyWereSacrificedToCnidathquaDuringTheCarnivale
  | TheSunBanishedCnidathquaIntoTheDepths
  | CnidathquaRetreatedToNurseItsWounds
  | -- | Murder at the Excelsior Hotel
    TheExcelsiorClaimsAnotherVictim
  | TheInvestigatorsFledTheSceneOfTheCrime
  | TheExcelsiorIsQuietForNow
  | TheMurdersContinueUnsolved
  | -- | Player Cards
    YouHaveIdentifiedTheSolution
  | YouHaveTranslatedTheGlyphs
  | YouHaveIdentifiedTheStone
  | DoomApproaches
  | TheHourIsNigh
  | YouHaveTranslatedTheTome
  | YouHaveInterpretedTheDreams
  | YouHaveTranslatedTheGrimoire
  | YouHaveIdentifiedTheGateway
  | YouHaveClassifiedANewSpecies
  | Teachings1
  | Teachings2
  | Teachings3
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''CampaignLogKey)

instance ToJSONKey CampaignLogKey
instance FromJSONKey CampaignLogKey

data Recorded a = Recorded a | CrossedOut a | Circled (Recorded a)
  deriving stock (Show, Ord, Eq)

instance ToJSON a => ToJSON (Recorded a) where
  toJSON (Recorded a) = object ["tag" .= String "Recorded", "contents" .= a]
  toJSON (CrossedOut a) = object ["tag" .= String "CrossedOut", "contents" .= a]
  toJSON (Circled a) = toJSON (a `with` Envelope @"circled" True)

instance FromJSON a => FromJSON (Recorded a) where
  parseJSON = withObject "Recorded" $ \o -> do
    isCircled <- o .:? "circled" .!= False
    let o' = KeyMap.delete "circled" o
    tag :: Text <- o' .: "tag"
    rec <- case tag of
      "Recorded" -> Recorded <$> o' .: "contents"
      "CrossedOut" -> CrossedOut <$> o' .: "contents"
      _ -> fail $ "Unknown tag: " <> T.unpack tag
    return $ if isCircled then Circled rec else rec

recordedCardCodes :: [SomeRecorded] -> [CardCode]
recordedCardCodes [] = []
recordedCardCodes (SomeRecorded RecordableCardCode (Recorded a) : as) = a : recordedCardCodes as
recordedCardCodes (SomeRecorded RecordableCardCode (Circled (Recorded a)) : as) = a : recordedCardCodes as
recordedCardCodes (_ : as) = recordedCardCodes as

unrecorded :: forall a. Recordable a => SomeRecorded -> Maybe a
unrecorded (SomeRecorded _ (rec :: Recorded b)) = case eqT @a @b of
  Just Refl -> case rec of
    Recorded a -> Just a
    Circled (Recorded a) -> Just a
    _ -> Nothing
  Nothing -> Nothing

instance ToGameLoggerFormat CampaignLogKey where
  format = \case
    Camp_CrashSite -> "Camp – Crash Site"
    Camp_FrozenShores -> "Camp – Frozen Shores"
    Camp_TreacherousPath -> "Camp – Treacherous Path"
    Camp_PrecariousIceSheet -> "Camp – Precarious Ice Sheet"
    Camp_BroadSnowdrifts -> "Camp – BroadSnowdrifts"
    Camp_IcyWastes -> "Camp – Icy Wastes"
    Camp_RockyCrags -> "Camp – Rocky Crags"
    Camp_SnowGraves -> "Camp – Snow Graves"
    Camp_IcebreakerLanding -> "Camp – Icebreaker Landing"
    Camp_FrigidCave -> "Camp – Frigid Cave"
    Camp_BarrierCamp -> "Camp – Barrier Camp"
    Camp_RemnantsOfLakesCamp -> "Camp – Remnants of Lake's Camp"
    Camp_CrystallineCavern -> "Camp – Crystalling Cavern"
    s -> pack . go $ show s
   where
    go :: String -> String
    go [] = []
    go (x : xs) = toLower x : go' xs

    go' :: String -> String
    go' [] = []
    go' (x : xs) | isUpper x = ' ' : toLower x : go' xs
    go' (x : xs) = x : go' xs

class (ToJSON a, FromJSON a, Eq a, Show a, Typeable a) => Recordable a where
  recordableType :: RecordableType a

instance Recordable CardCode where
  recordableType = RecordableCardCode

instance Recordable Memento where
  recordableType = RecordableMemento

instance Recordable Memory where
  recordableType = RecordableMemory

instance Recordable Value where
  recordableType = RecordableGeneric

recorded :: forall a. Recordable a => a -> SomeRecorded
recorded a = SomeRecorded (recordableType @a) (Recorded a)

circled :: forall a. Recordable a => a -> SomeRecorded
circled a = SomeRecorded (recordableType @a) (Circled (Recorded a))

crossedOut :: forall a. Recordable a => a -> SomeRecorded
crossedOut a = SomeRecorded (recordableType @a) (CrossedOut a)

data RecordableType a where
  RecordableCardCode :: RecordableType CardCode
  RecordableMemento :: RecordableType Memento
  RecordableMemory :: RecordableType Memory
  RecordableGeneric :: RecordableType Value

data SomeRecordableType where
  SomeRecordableType :: RecordableType a -> SomeRecordableType

deriving stock instance Show (RecordableType a)
deriving stock instance Eq (RecordableType a)

deriving stock instance Show SomeRecordableType

instance ToJSON (RecordableType a) where
  toJSON = toJSON . show

instance FromJSON SomeRecordableType where
  parseJSON = withText "RecordableType" $ \case
    "RecordableCardCode" -> pure $ SomeRecordableType RecordableCardCode
    "RecordableMemento" -> pure $ SomeRecordableType RecordableMemento
    "RecordableMemory" -> pure $ SomeRecordableType RecordableMemory
    "RecordableGeneric" -> pure $ SomeRecordableType RecordableGeneric
    other -> fail $ "No such recordable type: " <> unpack other

data SomeRecorded where
  SomeRecorded :: Recordable a => RecordableType a -> Recorded a -> SomeRecorded

deriving stock instance Show SomeRecorded

instance Data SomeRecorded where
  gunfold _ _ _ = error "gunfold(SomeRecorded)"
  toConstr _ = error "toConstr(SomeRecorded)"
  dataTypeOf _ = error "dataTypeOf(SomeRecorded)"

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
      SomeRecordableType RecordableMemory -> do
        rVal <- o .: "recordVal"
        pure $ SomeRecorded RecordableMemory rVal
      SomeRecordableType RecordableGeneric -> do
        rVal <- o .: "recordVal"
        pure $ SomeRecorded RecordableGeneric rVal
